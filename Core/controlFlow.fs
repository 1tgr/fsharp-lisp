#light
namespace Tim.Lisp.Core

open System
open System.Threading

module ControlFlow =
    open Scoped
    open Typed

    type NodeId = int

    type Instr<'a> = InitLocal of DeclId * 'a
                   | Eval of 'a

    type Node<'a> = Instr<'a> list

    type Edge<'a> = Always of NodeId
                  | IfEqual of 'a * 'a * NodeId * NodeId
                  | IfTrue of 'a * NodeId * NodeId

    type Graph<'a> =
        {
            Nodes : Map<NodeId, Node<'a>>
            OutEdges : Map<NodeId, Edge<'a>>
        }

    module Node =
        let empty = List.empty

        let singleton (instr : Instr<_>) : Node<_> =
            [instr]

        let add (instr : Instr<_>) (node : Node<_>) : Node<_> =
            node @ [instr]

    module Graph =
        let makeNodeId =
            let nextNodeId = ref 1
            fun () -> Interlocked.Increment(nextNodeId)

        let empty = { Graph.Nodes = Map.empty
                      OutEdges = Map.empty }

        let addNode (id : NodeId) (node : Node<_>) (graph : Graph<_>) : Graph<_> =
            { graph with Nodes = Map.add id node graph.Nodes }

        let addEdge (fromId : NodeId) (edge : Edge<'a>) (graph : Graph<_>) : Graph<_> =
            { graph with OutEdges = Map.add fromId edge graph.OutEdges }

        let appendToNode (node : Instr<_>) (exitNodes : NodeId list) (graph : Graph<_>) : NodeId list * Graph<_> =
            match exitNodes with
            | [id] ->
                let graph = addNode id (Node.add node graph.Nodes.[id]) graph
                [id], graph

            | ids ->
                let id = makeNodeId ()
                let graph = addNode id (Node.singleton node) graph
                let graph = List.fold (fun g fromId -> addEdge fromId (Always id) g) graph ids
                [id], graph

    let makeGraph (envs : Map<EnvId, Env<Expr, Type>>) (body : Stmt<Expr>) : NodeId * NodeId list * Graph<Expr> =
        let rec addToGraph
            (body      : Stmt<_>)
            (exitNodes : NodeId list)
            (graph     : Graph<_>)
                       : NodeId list * Graph<_>
            =
            match body with
            | Chain(head, tail) ->
                let exitNodes, graph = addToGraph head exitNodes graph
                addToGraph tail exitNodes graph

            | EnterEnv(envId, tail) ->
                let env = envs.[envId]

                let exitNodes, graph =
                    env.Values
                    |> Map.fold (fun (exitNodes, graph) _ value ->
                        match value with
                        | Var(varId, var) -> Graph.appendToNode (InitLocal(varId, var.InitExpr)) exitNodes graph
                        | _ -> exitNodes, graph) (exitNodes, graph)

                match tail with
                | Some tail -> addToGraph tail exitNodes graph
                | None -> exitNodes, graph
                
            | Expr(ApplyIfFunc(_, test, ifTrue, ifFalse)) ->
                let t = Graph.makeNodeId ()
                let f = Graph.makeNodeId ()

                let graph =
                    graph
                    |> Graph.addNode t (Node.singleton (Eval ifTrue))
                    |> Graph.addNode f (Node.singleton (Eval ifFalse))

                let edge =
                    match test with
                    | ApplyEqFunc(_, a, b) -> IfEqual(a, b, t, f)
                    | _ -> IfTrue(test, t, f)

                let graph =                        
                    match exitNodes with
                    | [id] ->
                        Graph.addEdge id edge graph

                    | ids ->
                        let id = Graph.makeNodeId ()
                        let graph = Graph.addNode id Node.empty graph

                        ids
                        |> List.fold (fun g fromId -> Graph.addEdge fromId (Always id) g) graph
                        |> Graph.addEdge id edge

                [t; f], graph

            | Expr e ->
                Graph.appendToNode (Eval e) exitNodes graph

        let entryNode = Graph.makeNodeId ()

        let exitNodes, graph =
            Graph.empty
            |> Graph.addNode entryNode Node.empty
            |> addToGraph body [entryNode]

        entryNode, exitNodes, graph
