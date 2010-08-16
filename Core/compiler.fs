#light
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Threading
open Tim.Lisp.Core.Syntax

module internal CompilerImpl =
    type DeclId = int

    type Block<'a> =
        {
            Body : Stmt<'a> list
        }
        static member empty : Block<'a> = { Body = List.empty }

    and Stmt<'a> = Block of Env<'a> * Block<'a>
                 | Expr of Env<'a> * Expr<'a>

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

    let nextDeclId : unit -> DeclId =
        let id = ref 0
        fun () -> Interlocked.Increment(id)

    let rec makeBlock (env : Env<'a>) (exprs : Expr<'a> list) : Env<'a> * Block<'a> =
        let rec addToScope
            (env   : Env<'a>, block : Block<'a>) 
            (exprs : Expr<'a> list) 
                   : Env<'a> * Block<'a>
            =
            match exprs with
            | List(_, Atom(_, "define") :: values) :: tail ->
                let name, value = 
                    match values with
                    | [Atom(_, name); value] ->
                        let var = { DeclEnv = env
                                    InitExpr = value }

                        name, Var(nextDeclId (), var)

                    | List(_, Atom(_, name) :: atoms) :: values ->
                        let nameOfAtom =
                            function
                            | Atom(_, name) -> name
                            | _ -> failwith "expected atom"

                        let paramNames = List.map nameOfAtom atoms
                        let blockRef = ref { Body = List.empty }
                        let func = { Block = blockRef
                                     Params = paramNames }

                        let id = nextDeclId()
                        let envValues = (name, Func(id, func)) :: List.mapi (fun i name -> name, Arg i) paramNames

                        let funcEnv = { Parent = Some env
                                        Func = id
                                        Values = Map.ofList envValues }

                        let _, block = makeBlock funcEnv values
                        blockRef := block
                        name, Func(id, func)

                    | _ ->
                        failwithf "define expected 1 value, not %A" values

                match block with
                | { Body = [] } ->
                    let env = { env with Values = Map.add name value env.Values }
                    tail |> addToScope (env, block)

                | _ ->
                    let ienv = { env with Parent = Some env
                                          Values = Map.ofList [(name, value)] }

                    let ienv, iblock = tail |> addToScope (ienv, { Body = [] })
                    env, { block with Body = block.Body @ [Block(ienv, iblock)] }

            | head :: tail ->
                let scope = env, { block with Body = block.Body @ [Expr(env, head)] }
                tail |> addToScope scope

            | [] ->
                env, block

        addToScope (env, { Body = [] }) exprs

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
        | Block(env, block) :: _ -> blockType block
        | Expr(env, expr) :: _ -> exprType env expr

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

    and makeILFunctions (typeBuilder : TypeBuilder) (map : Map<DeclId, ILFunction<'a>>) (stmt : Stmt<'a>) : Map<DeclId, ILFunction<'a>> =
        match stmt with
        | Block(env, block) ->
            match List.rev block.Body with
            | lastStmt :: _ -> makeILFunctions typeBuilder map lastStmt
            | [] -> map

        | Expr(env, _) ->
            Map.fold (makeILFunctionsFromValue typeBuilder) map env.Values

    let emitFunc (ilFuncs : Map<DeclId, ILFunction<'a>>) (ilFunc : ILFunction<'a>) : unit =
        let g = ilFunc.Generator

        let rec call (mi : MethodInfo) (env : Env<_>) (args : Expr<_> list) : unit =
            for arg in args do
                emitExpr env arg

            g.Emit(OpCodes.Call, mi)

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

                | List(_, Atom(_, name) :: args) ->
                    match lookup name env with
                    | Func(id, _) ->
                        match Map.tryFind id ilFuncs with
                        | Some ilFunc ->
                            call ilFunc.DynamicMethod env args

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
                        call mi env args

                    | Arg _ | Var _ ->
                        failwith "delegates not yet implemented"

                | List _ ->
                    failwith "quotation not yet implemented"

                | String(_, s) ->
                    g.Emit(OpCodes.Ldstr, s)

            with ex ->
                raise <| InvalidOperationException(sprintf "%s at %A" ex.Message (Expr.annot expr), ex)

        let rec emitStmt (stmt : Stmt<_>) : unit =
            match stmt with
            | Block(_, block) ->
                List.iter emitStmt block.Body

            | Expr(env, expr) ->
                emitExpr env expr

        List.iter emitStmt (!ilFunc.Func.Block).Body
        g.Emit(OpCodes.Ret)

    let wrap2 (f : 'a -> 'b -> 'c) : MethodInfo =
        (new System.Func<'a, 'b, 'c>(f)).Method

    let wrap3 (f : 'a -> 'b -> 'c -> 'd) : MethodInfo =
        (new System.Func<'a, 'b, 'c, 'd>(f)).Method

open CompilerImpl

module Compiler =
    let compileToDelegate (delegateType : Type) (code : Expr<'a> list) : Delegate =
        let main  =
            let values =
                ["Console.WriteLine", NetFunc <| typeof<Console>.GetMethod("WriteLine", Array.empty)
                 "+",                 NetFunc <| wrap2(fun a b -> a + b)
                 "-",                 NetFunc <| wrap2(fun a b -> a - b)
                 "*",                 NetFunc <| wrap2(fun a b -> a * b)
                 "/",                 NetFunc <| wrap2(fun a b -> a / b)
                 "if",                IfFunc]
                |> Map.ofList

            let id = nextDeclId ()
            let mainEnv, mainBlock = makeBlock { Parent = None; Func = id; Values = values } code
            let main = { Block = ref mainBlock; Params = List.empty }
            Func(id, main)

        let name = AssemblyName("DynamicAssembly")
        let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(name.Name + ".dll")
        let typeBuilder = moduleBuilder.DefineType("Program")
        let ilFuncs = makeILFunctionsFromValue typeBuilder Map.empty "main" main

        for (_, ilFunc) in Map.toSeq ilFuncs do
            emitFunc ilFuncs ilFunc

        let t = typeBuilder.CreateType()
        assemblyBuilder.Save(name.Name + ".dll")
        let methodInfo = t.GetMethod("main", BindingFlags.Public ||| BindingFlags.Static)
        Delegate.CreateDelegate(delegateType, methodInfo)
