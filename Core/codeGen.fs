#light
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit

module CodeGen =
    open Asm
    open Scoped
    open Typed

    type ILFunction<'a>(id : DeclId,
                        func : Func<Expr<'a>>,
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

    let rec makeILFunctionsFromValue (typeBuilder : TypeBuilder) (map : Map<DeclId, ILFunction<_>>) (name : string) (value : EnvValue<_>) : Map<DeclId, ILFunction<_>> =
        match value with
        | Func(id, func) ->
            let ilFunc = new ILFunction<_>(id, func, typeBuilder, name)
            let map = Map.add id ilFunc map
            makeILFunctionsFromEnv typeBuilder map (!func.Block).Env

        | _ -> map

    and makeILFunctionsFromEnv (typeBuilder : TypeBuilder) (map : Map<DeclId, ILFunction<_>>) (env : Env<_>) : Map<DeclId, ILFunction<_>> =
        Map.fold (makeILFunctionsFromValue typeBuilder) map env.Values

    and makeILFunctions (typeBuilder : TypeBuilder) (map : Map<DeclId, ILFunction<_>>) (stmt : Stmt<_>) : Map<DeclId, ILFunction<_>> =
        match stmt with
        | Block block -> List.fold (makeILFunctions typeBuilder) (makeILFunctionsFromEnv typeBuilder map block.Env) block.Body
        | _ -> map

    let emitFunc (ilFuncs : Map<DeclId, ILFunction<_>>) (ilFunc : ILFunction<_>) : unit =
        let g = ilFunc.Generator

        let rec emitCall (mi : MethodInfo) (args : Expr<_> list) : unit =
            for arg in args do
                emitExpr arg

            g.Emit(OpCodes.Call, mi)

        and emitAsm (asm : Asm<_>) : unit =
            for stack in asm.Stack do
                emitExpr stack

            let types, args =
                match asm.Operand with
                | None -> [| typeof<OpCode> |], [| box asm.OpCode |]
                | Some o -> [| typeof<OpCode>; o.GetType() |], [| box asm.OpCode; o |]

            match g.GetType().GetMethod("Emit", BindingFlags.Public ||| BindingFlags.Instance, null, types, null) with
            | null -> failwithf "can't emit operand %A" asm.Operand
            | mi -> ignore <| mi.Invoke(g, args)

        and emitExpr (expr : Expr<_>) : unit =
            match expr with
            | ApplyFunc(_, _, id, args) ->
                match Map.tryFind id ilFuncs with
                | Some ilFunc ->
                    emitCall ilFunc.DynamicMethod args

                | None ->
                    failwithf "no ILFunction for id = %d" id

            | ApplyIfFunc(_, test, ifTrue, ifFalse) ->
                match ifTrue with
                | ApplyFunc(_, _, equalFunc, [left; right]) ->
                    let eqLabel = g.DefineLabel()
                    let endLabel = g.DefineLabel()
                    emitExpr left
                    emitExpr right
                    g.Emit(OpCodes.Beq, eqLabel)
                    emitExpr ifFalse
                    g.Emit(OpCodes.Br, endLabel)
                    g.MarkLabel(eqLabel)
                    emitExpr ifTrue
                    g.MarkLabel(endLabel)

                | _ ->
                    let elseLabel = g.DefineLabel()
                    let endLabel = g.DefineLabel()
                    emitExpr test
                    g.Emit(OpCodes.Brfalse, elseLabel)
                    emitExpr ifTrue
                    g.Emit(OpCodes.Br, endLabel)
                    g.MarkLabel(elseLabel)
                    emitExpr ifFalse
                    g.MarkLabel(endLabel)

            | ApplyNetFunc(_, mi, args) ->
                emitCall mi args

            | Asm(_, asm) ->
                emitAsm asm
            
            | Float(_, n) ->
                g.Emit(OpCodes.Ldc_R4, n)

            | Int(_, n) ->
                g.Emit(OpCodes.Ldc_I4, n)

            | LookupArg(_, _, i) ->
                g.Emit(OpCodes.Ldarg, i)

            | LookupVar(_, _, id) ->
                // TODO
                ()

            | String(_, s) ->
                g.Emit(OpCodes.Ldstr, s)

        let rec emitBlock (block : Block<_>) : unit =
            List.iter emitStmt block.Body

        and emitStmt (stmt : Stmt<_>) : unit =
            match stmt with
            | Block block -> emitBlock block
            | Expr expr -> emitExpr expr

        emitBlock !ilFunc.Func.Block
        g.Emit(OpCodes.Ret)

