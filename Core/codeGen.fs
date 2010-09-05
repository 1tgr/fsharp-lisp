#light
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit

module CodeGen =
    open Asm
    open ControlFlow
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

        do
            let mutable index = 1
            for name in func.Params do
                ignore <| dynamicMethod.DefineParameter(index, ParameterAttributes.None, name)
                index <- index + 1

        let generator = dynamicMethod.GetILGenerator()

        member this.Id = id
        member this.Func = func
        member this.TypeBuilder = typeBuilder
        member this.Name = name
        member this.DynamicMethod = dynamicMethod
        member this.Generator = generator

    let makeILFunction (typeBuilder : TypeBuilder) (map : Map<DeclId, ILFunction<_>>) (name : string) (value : EnvValue<_>) : Map<DeclId, ILFunction<_>> =
        match value with
        | Func(id, func) ->
            let ilFunction = new ILFunction<_>(id, func, typeBuilder, name)
            Map.add id ilFunction map

        | _ -> map

    let makeLocal (g : ILGenerator) (map : Map<DeclId, LocalBuilder>) (name : string) (value : EnvValue<_>) : Map<DeclId, LocalBuilder> =
        match value with
        | Var(id, var) ->
            let t = exprType var.InitExpr
            let local = g.DeclareLocal(t)
            Map.add id local map

        | _ -> map

    let emitFunc (ilFuncs : Map<DeclId, ILFunction<_>>) (ilFunc : ILFunction<_>) : unit =
        let g = ilFunc.Generator
        let funcBlock = !ilFunc.Func.Block
        let locals = foldEnv (makeLocal g) Map.empty funcBlock.Env

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
            | ApplyEqFunc(_, x, y) -> failwith "can't call = directly"

            | ApplyFunc(_, _, id, args) ->
                match Map.tryFind id ilFuncs with
                | Some ilFunc -> emitCall ilFunc.DynamicMethod args
                | None -> failwithf "no ILFunction for id = %d" id

            | ApplyIfFunc _-> failwith "didn't expect to find ApplyIfFunc in control flow graph"
            | ApplyNetFunc(_, mi, args) -> emitCall mi args
            | Asm(_, asm) -> emitAsm asm
            | Float(_, n) -> g.Emit(OpCodes.Ldc_R4, n)
            | Int(_, n) -> g.Emit(OpCodes.Ldc_I4, n)
            | LookupArg(_, _, i) -> g.Emit(OpCodes.Ldarg, i)
            | LookupVar(_, _, id) -> g.Emit(OpCodes.Ldloc, locals.[id])
            | String(_, s) -> g.Emit(OpCodes.Ldstr, s)

        let emitLocal (value : EnvValue<_>) : unit =
            match value with
            | Var(id, var) ->
                let local = locals.[id]
                emitExpr var.InitExpr
                g.Emit(OpCodes.Stloc, local)

            | _ ->
                ()

        let rec emitBlock (block : Block<_>) : unit =
            for _, value in Map.toSeq block.Env.Values do
                emitLocal value

            for stmt in block.Body do
                emitStmt stmt

        and emitStmt (stmt : Stmt<_>) : unit  =
            match stmt with
            | Block block ->
                emitBlock block

            | Expr expr ->
                emitExpr expr

        for _, value in Map.toSeq funcBlock.Env.Values do
            emitLocal value

        let entryNode, exitNodes, graph = graph !ilFunc.Func.Block
        let labels = Map.map (fun _ _ -> g.DefineLabel()) graph.Nodes

        for nodeId, node in Map.toSeq graph.Nodes do
            g.MarkLabel(labels.[nodeId])

            for stmt in node do
                emitStmt stmt

            match Map.tryFind nodeId graph.OutEdges with
            | None ->
                g.Emit(OpCodes.Ret)

            | Some (Always toId) ->
                g.Emit(OpCodes.Br, labels.[toId])

            | Some (IfEqual(a, b, eqId, neqId)) ->
                emitExpr a
                emitExpr b
                g.Emit(OpCodes.Beq, labels.[eqId])
                g.Emit(OpCodes.Br, labels.[neqId])

            | Some (IfTrue(test, trueId, falseId)) ->
                emitExpr test
                g.Emit(OpCodes.Brtrue, labels.[trueId])
                g.Emit(OpCodes.Br, labels.[falseId])
