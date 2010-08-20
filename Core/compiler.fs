#light
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Threading
open Tim.Lisp.Core.Asm
open Tim.Lisp.Core.Syntax

module internal CompilerImpl =
    type DeclId = int

    type Block<'a> =
        {
            Env : Env<'a>
            Body : Stmt<'a> list
        }
        static member empty : Block<'a> = { Env = Env.empty; Body = List.empty }

    and Stmt<'a> = Block of Block<'a>
                 | Expr of Expr<'a>

    and Func<'a> =
        {
            Block : Block<'a> ref
            Params : string list
        }

    and Var<'a> =
        {
            DeclEnv : Env<'a>
            InitExpr : Expr<'a>
        }

    and EnvValue<'a> = Func of DeclId * Func<'a>
                     | IfFunc
                     | NetFunc of MethodInfo
                     | Arg of int
                     | Var of DeclId * Var<'a>

    and Env<'a> =
        {
            Parent : Env<'a> option
            Func : DeclId
            Values : Map<string, EnvValue<'a>>
        }
        static member empty : Env<'a> = { Parent = None; Func = 0; Values = Map.empty }

    let nextDeclId : unit -> DeclId =
        let id = ref 0
        fun () -> Interlocked.Increment(id)

    let rec makeFunc (env : Env<_>) (name : string) (paramNames : string list) (body : Expr<_> list) : DeclId * Func<_> =
        let blockRef = ref Block.empty
        let func = { Block = blockRef
                     Params = paramNames }

        let id = nextDeclId()
        let envValues = (name, Func(id, func)) :: List.mapi (fun i name -> name, Arg i) paramNames

        let funcEnv = { Parent = Some env
                        Func = id
                        Values = Map.ofList envValues }

        blockRef := makeBlock funcEnv body
        id, func

    and makeBlock (parentEnv : Env<'a>) (exprs : Expr<'a> list) : Block<'a> =
        let rec addToBlock
            (block : Block<'a>) 
            (exprs : Expr<'a> list) 
                   : Block<'a>
            =
            match exprs with
            | List(_, Atom(_, "define") :: values) :: tail ->
                let name, value = 
                    match values with
                    | [Atom(_, name); value] ->
                        let var = { DeclEnv = block.Env
                                    InitExpr = value }

                        name, Var(nextDeclId (), var)

                    | List(_, Atom(_, name) :: atoms) :: body ->
                        let nameOfAtom =
                            function
                            | Atom(_, name) -> name
                            | _ -> failwith "expected atom"

                        let paramNames = List.map nameOfAtom atoms
                        let id, func = makeFunc block.Env name paramNames body
                        name, Func(id, func)

                    | _ ->
                        failwithf "define expected 1 value, not %A" values

                match block with
                | { Body = [] } ->
                    let env = block.Env
                    let env = { env with Values = Map.add name value env.Values }
                    tail |> addToBlock { block with Env = env }

                | _ ->
                    let ienv = { block.Env with Parent = Some block.Env
                                                Values = Map.ofList [(name, value)] }

                    let iblock = tail |> addToBlock { Block.empty with Env = ienv }
                    { block with Body = block.Body @ [Block iblock] }

            | head :: tail ->
                tail |> addToBlock { block with Body = block.Body @ [Expr head] }

            | [] ->
                block

        addToBlock { Block.empty with Env = parentEnv } exprs

    let rec lookup (name : string) (env : Env<'a>) : EnvValue<'a> =
        match Map.tryFind name env.Values, env.Parent with
        | Some value, _ -> value
        | None, Some parent -> lookup name parent
        | None, None -> failwithf "undeclared identifier %s" name

    let rec exprType (env : Env<'a>) (expr : Expr<_>) : Type =
        match expr with
        | Atom(_, name) ->
            match lookup name env with
            | Arg _ -> typeof<int>
            | Var(_, var) -> exprType var.DeclEnv var.InitExpr
            | _ -> failwith "delegates not yet implemented"

        | Float _ ->
            typeof<float>

        | Int _ ->
            typeof<int>

        | List(_, Atom(_, ".asm") :: args) ->
            (parseAsm args).ResultType

        | List(_, Atom(_, name) :: args) ->
            match lookup name env with
            | Func(_, func) ->
                // TODO: type inference for recursive functions
                //blockType !func.Block
                typeof<int>

            | IfFunc ->
                match args with
                | [test; ifTrue; ifFalse] -> exprType env ifTrue
                | _ -> failwith "expected 3 args for if, not %A" args

            | NetFunc mi ->
                mi.ReturnType

            | Arg _ | Var _ ->
                failwith "delegates not yet implemented"

        | List _ ->
            failwith "quotation not yet implemented"

        | String(_, s) ->
            typeof<string>

    and blockType (block : Block<_>) : Type =
        match List.rev block.Body with
        | [] -> typeof<Void>
        | Block block :: _ -> blockType block
        | Expr expr :: _ -> exprType block.Env expr

    type ILFunction<'a>(id : DeclId,
                        func : Func<'a>,
                        typeBuilder : TypeBuilder, 
                        name : string)
        =
        let dynamicMethod = 
            typeBuilder.DefineMethod(
                name, 
                MethodAttributes.Public ||| MethodAttributes.Static, 
                blockType !func.Block, 
                func.Params |> List.map (fun _ -> typeof<int>) |> Array.ofList)

        let generator = dynamicMethod.GetILGenerator()

        member this.Id = id
        member this.Func = func
        member this.TypeBuilder = typeBuilder
        member this.Name = name
        member this.DynamicMethod = dynamicMethod
        member this.Generator = generator

    let rec makeILFunctionsFromValue (typeBuilder : TypeBuilder) (map : Map<DeclId, ILFunction<'a>>) (name : string) (value : EnvValue<'a>) : Map<DeclId, ILFunction<'a>> =
        match value with
        | Func(id, func) ->
            if (Map.containsKey id map) then
                map
            else
                let ilFunc = new ILFunction<'a>(id, func, typeBuilder, name)
                let map = Map.add id ilFunc map
                match List.rev (!func.Block).Body with
                | lastStmt :: _ -> makeILFunctions typeBuilder map lastStmt
                | [] -> map

        | _ -> map

    and makeILFunctionsFromEnv (typeBuilder : TypeBuilder) (map : Map<DeclId, ILFunction<'a>>) (env : Env<'a>) : Map<DeclId, ILFunction<'a>> =
        Map.fold (makeILFunctionsFromValue typeBuilder) map env.Values

    and makeILFunctions (typeBuilder : TypeBuilder) (map : Map<DeclId, ILFunction<'a>>) (stmt : Stmt<'a>) : Map<DeclId, ILFunction<'a>> =
        match stmt with
        | Block block -> List.fold (makeILFunctions typeBuilder) (makeILFunctionsFromEnv typeBuilder map block.Env) block.Body
        | _ -> map

    let emitFunc (ilFuncs : Map<DeclId, ILFunction<'a>>) (ilFunc : ILFunction<'a>) : unit =
        let g = ilFunc.Generator

        let rec emitCall (mi : MethodInfo) (env : Env<_>) (args : Expr<_> list) : unit =
            for arg in args do
                emitExpr env arg

            g.Emit(OpCodes.Call, mi)

        and emitAsm (env : Env<_>) (asm : Asm<_>) : unit =
            for stack in asm.Stack do
                emitExpr env stack

            let types, args =
                match asm.Operand with
                | None -> [| typeof<OpCode> |], [| box asm.OpCode |]
                | Some o -> [| typeof<OpCode>; o.GetType() |], [| box asm.OpCode; o |]

            match g.GetType().GetMethod("Emit", BindingFlags.Public ||| BindingFlags.Instance, null, types, null) with
            | null -> failwithf "can't emit operand %A" asm.Operand
            | mi -> ignore <| mi.Invoke(g, args)

        and emitExpr (env : Env<_>) (expr : Expr<_>) : unit =
            try
                match expr with
                | Atom(_, name) ->
                    match lookup name env with
                    | Arg i -> g.Emit(OpCodes.Ldarg, i)
                    | Var(_, var) -> emitExpr var.DeclEnv var.InitExpr
                    | _ -> failwith "delegates not yet implemented"

                | Float(_, n) ->
                    g.Emit(OpCodes.Ldc_R4, n)

                | Int(_, n) ->
                    g.Emit(OpCodes.Ldc_I4, n)

                | List(_, Atom(_, ".asm") :: args) ->
                    emitAsm env (parseAsm args)

                | List(_, Atom(_, name) :: args) ->
                    match lookup name env with
                    | Func(id, _) ->
                        match Map.tryFind id ilFuncs with
                        | Some ilFunc ->
                            emitCall ilFunc.DynamicMethod env args

                        | None ->
                            failwithf "no ILFunction for name = %s, id = %d" name id

                    | IfFunc ->
                        match args with
                        | [List(_, [Atom(_, "="); left; right]); ifEqual; ifNotEqual] ->
                            let eqLabel = g.DefineLabel()
                            let endLabel = g.DefineLabel()
                            emitExpr env left
                            emitExpr env right
                            g.Emit(OpCodes.Beq, eqLabel)
                            emitExpr env ifNotEqual
                            g.Emit(OpCodes.Br, endLabel)
                            g.MarkLabel(eqLabel)
                            emitExpr env ifEqual
                            g.MarkLabel(endLabel)

                        | [test; ifTrue; ifFalse] ->
                            let elseLabel = g.DefineLabel()
                            let endLabel = g.DefineLabel()
                            emitExpr env test
                            g.Emit(OpCodes.Brfalse, elseLabel)
                            emitExpr env ifTrue
                            g.Emit(OpCodes.Br, endLabel)
                            g.MarkLabel(elseLabel)
                            emitExpr env ifFalse
                            g.MarkLabel(endLabel)

                        | _ ->
                            failwith "expected 3 args for if, not %A" args

                    | NetFunc mi ->
                        emitCall mi env args

                    | Arg _ | Var _ ->
                        failwith "delegates not yet implemented"

                | List _ ->
                    failwith "quotation not yet implemented"

                | String(_, s) ->
                    g.Emit(OpCodes.Ldstr, s)

            with ex ->
                raise <| InvalidOperationException(sprintf "%s at %A" ex.Message (Expr.annot expr), ex)

        let rec emitBlock (block : Block<_>) : unit =
            List.iter (emitStmt block.Env) block.Body

        and emitStmt (env : Env<_>) (stmt : Stmt<_>) : unit =
            match stmt with
            | Block block -> emitBlock block
            | Expr expr -> emitExpr env expr

        emitBlock !ilFunc.Func.Block
        g.Emit(OpCodes.Ret)

open CompilerImpl

module Compiler =
    let compileToDelegate (delegateType : Type) (code : Expr<'a> list) : Delegate =
        let main =
            let values =
                ["Console.WriteLine", NetFunc <| typeof<Console>.GetMethod("WriteLine", Array.empty)
                 "if",                IfFunc]
                |> Map.ofList

            let _, main = makeFunc { Env.empty with Values = values } "main" List.empty code
            main

        let name = AssemblyName("DynamicAssembly")
        let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(name.Name + ".dll")
        let typeBuilder = moduleBuilder.DefineType("Program")
        let ilFuncs = makeILFunctionsFromEnv typeBuilder Map.empty (!main.Block).Env

        for (_, ilFunc) in Map.toSeq ilFuncs do
            emitFunc ilFuncs ilFunc

        let t = typeBuilder.CreateType()
        assemblyBuilder.Save(name.Name + ".dll")
        let methodInfo = t.GetMethod("main", BindingFlags.Public ||| BindingFlags.Static)
        Delegate.CreateDelegate(delegateType, methodInfo)
