#light
namespace Tim.Lisp.Core

open System.Threading

module ControlFlow =
    open Scoped
    open Typed

    type NodeId = int

    type Node<'a> = Stmt<'a> list

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

        let singleton (stmt : Stmt<_>) : Node<_> =
            [stmt]

        let add (stmt : Stmt<_>) (node : Node<_>) : Node<_> =
            node @ [stmt]

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

    let makeGraph (block : Block<Expr<_>>) : NodeId * NodeId list * Graph<Expr<_>> =
        let rec addToGraph
            (body      : Stmt<_> list)
            (exitNodes : NodeId list)
            (graph     : Graph<_>)
                       : NodeId list * Graph<_>
            =
            match body with
            | [] ->
                exitNodes, graph

            | head :: tail ->
                match head with
                | Expr(ApplyIfFunc(_, test, ifTrue, ifFalse)) ->
                    let t = Graph.makeNodeId ()
                    let f = Graph.makeNodeId ()

                    let graph =
                        graph
                        |> Graph.addNode t (Node.singleton (Expr ifTrue))
                        |> Graph.addNode f (Node.singleton (Expr ifFalse))

                    let edge =
                        match test with
                        | ApplyEqFunc(_, a, b) -> IfEqual(a, b, t, f)
                        | _ -> IfTrue(test, t, f)
                        
                    match exitNodes with
                    | [id] ->
                        graph
                        |> Graph.addEdge id edge
                        |> addToGraph tail [t; f]

                    | ids ->
                        let id = Graph.makeNodeId ()
                        let graph = Graph.addNode id Node.empty graph

                        ids
                        |> List.fold (fun g fromId -> Graph.addEdge fromId (Always id) g) graph
                        |> Graph.addEdge id edge
                        |> addToGraph tail [t; f]

                | stmt ->
                    match exitNodes with
                    | [id] ->
                        graph
                        |> Graph.addNode id (Node.add stmt graph.Nodes.[id])
                        |> addToGraph tail [id]

                    | ids ->
                        let id = Graph.makeNodeId ()
                        let graph = Graph.addNode id (Node.singleton stmt) graph

                        ids
                        |> List.fold (fun g fromId -> Graph.addEdge fromId (Always id) g) graph
                        |> addToGraph tail [id]

        let entryNode = Graph.makeNodeId ()

        let exitNodes, graph =
            Graph.empty
            |> Graph.addNode entryNode Node.empty
            |> addToGraph block.Body [entryNode]

        entryNode, exitNodes, graph