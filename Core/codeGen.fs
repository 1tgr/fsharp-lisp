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

    type ILContext<'a> =
        {
            ILFuncs : Map<DeclId, ILFunction<'a>>
            IsTail : bool
        }

    and ILFunction<'a>(func : Func<Expr, Type>,
                       envs : Map<EnvId, Env<Expr, Type>>,
                       typeBuilder : TypeBuilder, 
                       name : string)
        =
        let dynamicMethod = 
            typeBuilder.DefineMethod(
                name, 
                MethodAttributes.Public ||| MethodAttributes.Static, 
                blockType func.Block, 
                func.Params |> List.map snd |> Array.ofList)

        do
            let mutable index = 1
            for name, _ in func.Params do
                ignore <| dynamicMethod.DefineParameter(index, ParameterAttributes.None, name)
                index <- index + 1

        let g = dynamicMethod.GetILGenerator()

        let makeLocal (map : Map<DeclId, LocalBuilder>) (name : string) (value : EnvValue<_, _>) : Map<DeclId, LocalBuilder> =
            match value with
            | Var(id, var) ->
                let t = exprType var.InitExpr
                let local = g.DeclareLocal(t)
                Map.add id local map

            | _ -> map

        let locals = Map.fold makeLocal Map.empty envs.[func.Block.Env].Values

        member this.Func = func
        member this.DynamicMethod = dynamicMethod
        member this.Generator = g

        member this.EmitCall (context : ILContext<_>) (mi : MethodInfo) (args : Expr list) : unit =
            let noTailCall = { context with IsTail = false }
            for arg in args do
                this.EmitExpr noTailCall arg

            if context.IsTail then
                g.Emit(OpCodes.Tailcall)

            g.Emit(OpCodes.Call, mi)

        member this.EmitAsm (context : ILContext<_>) (asm : Asm<_>) : unit =
            let noTailCall = { context with IsTail = false }
            for stack in asm.Stack do
                this.EmitExpr noTailCall stack

            let isCall = asm.OpCode = OpCodes.Call || 
                         asm.OpCode = OpCodes.Calli || 
                         asm.OpCode = OpCodes.Callvirt

            if context.IsTail && isCall then
                g.Emit(OpCodes.Tailcall)

            let types, args =
                match asm.Operand with
                | None -> [| typeof<OpCode> |], [| box asm.OpCode |]
                | Some o -> [| typeof<OpCode>; o.GetType() |], [| box asm.OpCode; o |]

            match g.GetType().GetMethod("Emit", BindingFlags.Public ||| BindingFlags.Instance, null, types, null) with
            | null -> failwithf "can't emit operand %A" asm.Operand
            | mi -> ignore <| mi.Invoke(g, args)

        member this.EmitExpr (context : ILContext<_>) (expr : Expr) : unit =
            match expr with
            | ApplyEqFunc(_, x, y) -> failwith "can't call = directly"

            | ApplyFunc(_, _, id, args) ->
                match Map.tryFind id context.ILFuncs with
                | Some ilFunc -> this.EmitCall context ilFunc.DynamicMethod args
                | None -> failwithf "no ILFunction for id = %d" id

            | ApplyIfFunc _-> failwith "didn't expect to find ApplyIfFunc in control flow graph"
            | ApplyNetFunc(_, mi, args) -> this.EmitCall context mi args
            | Asm(_, asm) -> this.EmitAsm context asm
            | Bool(_, true) -> g.Emit(OpCodes.Ldc_I4_1)
            | Bool(_, false) -> g.Emit(OpCodes.Ldc_I4_0)
            | Char(_, c) -> g.Emit(OpCodes.Ldc_I4, int32 c)
            | Float(_, n) -> g.Emit(OpCodes.Ldc_R4, n)
            | Int(_, n) -> g.Emit(OpCodes.Ldc_I4, n)
            | LookupArg(_, _, i) -> g.Emit(OpCodes.Ldarg, i)
            | LookupVar(_, _, id) -> g.Emit(OpCodes.Ldloc, locals.[id])
            | String(_, s) -> g.Emit(OpCodes.Ldstr, s)

        member this.EmitInitExpr (context : ILContext<_>) (value : EnvValue<_, _>) : unit =
            match value with
            | Var(id, var) ->
                let local = locals.[id]
                this.EmitExpr { context with IsTail = false } var.InitExpr
                g.Emit(OpCodes.Stloc, local)

            | _ ->
                ()

        member this.EmitEnv (context : ILContext<_>) (env : EnvId) : unit =
            for _, value in Map.toSeq envs.[env].Values do
                this.EmitInitExpr context value

        member this.EmitBlock (context : ILContext<_>) (block : Block<_>) : unit =
            this.EmitEnv context block.Env

            for stmt in block.Body do
                this.EmitStmt context stmt

        member this.EmitStmt (context : ILContext<_>) (stmt : Stmt<_>) : unit =
            match stmt with
            | Block block -> this.EmitBlock context block
            | Expr expr -> this.EmitExpr context expr

    let makeILFunction
        (envs        : Map<EnvId, Env<_, _>>)
        (typeBuilder : TypeBuilder)
        (map         : Map<DeclId, ILFunction<_>>)
        (name        : string)
        (func        : EnvValue<_, _>)
                     : Map<DeclId, ILFunction<_>>
        =
        match func with
        | Func(id, func) ->
            let ilFunction = new ILFunction<_>(func, envs, typeBuilder, name)
            Map.add id ilFunction map

        | _ ->
            map

    let rec makeAdjacency (nodes : NodeId list) (adj : Map<NodeId, NodeId>) =
        match nodes with
        | first :: second :: rest ->
            adj
            |> Map.add first second
            |> makeAdjacency (second :: rest)

        | _ ->
            adj

    let emitFunc (ilFuncs : Map<DeclId, ILFunction<_>>) (ilFunc : ILFunction<_>) : unit =
        let g = ilFunc.Generator
        let funcBlock = ilFunc.Func.Block
        let entryNode, _, graph = makeGraph funcBlock
        let labels = Map.map (fun _ _ -> g.DefineLabel()) graph.Nodes

        let noTailCall = { ILContext.ILFuncs = ilFuncs
                           IsTail = false }

        let nodes =
            graph.Nodes
            |> Map.remove entryNode
            |> Map.toList

        let adj = makeAdjacency (List.map fst nodes) Map.empty

        let isAdjacent (first : NodeId) (second : NodeId) : bool =
            match Map.tryFind first adj with
            | Some adjacentTo when adjacentTo = second -> true
            | _ -> false

        let isTail (id : NodeId) : bool =
            if Map.containsKey id graph.OutEdges then
                false
            else
                match List.rev nodes with
                | [] when id = entryNode -> true
                | (tail, _) :: _ -> id = tail
                | _ -> false

        let emitNode (nodeId : NodeId) (node : Node<_>) : unit =
            let rec emitStmts (stmts : Stmt<_> list) : unit =
                match stmts with
                | [stmt] ->
                    ilFunc.EmitStmt { noTailCall with IsTail = isTail nodeId } stmt

                | head :: tail ->
                    ilFunc.EmitStmt noTailCall head
                    emitStmts tail

                | [] ->
                    ()

            g.MarkLabel(labels.[nodeId])
            emitStmts node

            match Map.tryFind nodeId graph.OutEdges with
            | None ->
                g.Emit(OpCodes.Ret)

            | Some (Always toId) ->
                if not (isAdjacent nodeId toId) then
                    g.Emit(OpCodes.Br, labels.[toId])

            | Some (IfEqual(a, b, eqId, neqId)) ->
                ilFunc.EmitExpr noTailCall a
                ilFunc.EmitExpr noTailCall b
                g.Emit(OpCodes.Beq, labels.[eqId])
                if not (isAdjacent nodeId neqId) then
                    g.Emit(OpCodes.Br, labels.[neqId])

            | Some (IfTrue(test, trueId, falseId)) ->
                ilFunc.EmitExpr noTailCall test
                g.Emit(OpCodes.Brtrue, labels.[trueId])
                if not (isAdjacent nodeId falseId) then
                    g.Emit(OpCodes.Br, labels.[falseId])

        ilFunc.EmitEnv noTailCall funcBlock.Env
        emitNode entryNode graph.Nodes.[entryNode]

        for nodeId, node in nodes do
            emitNode nodeId node
