#light
namespace Tim.Lisp.Core

open System.Threading

module ControlFlow =
    open Scoped
    open Typed

    type NodeId = int

    type Node<'a>(body : Stmt<'a> list) =
        member this.Body = body

        static member Empty : Node<'a> = new Node<'a>(List.empty)

    type Edge<'a> = IfEqual of 'a * 'a * NodeId * NodeId
                  | IfTrue of 'a * NodeId * NodeId
                  | Always of NodeId

    type Graph<'a> =
        {
            Nodes : Map<NodeId, Node<'a>>
            OutEdges : Map<NodeId, Edge<'a>>
        }
        static member Empty : Graph<'a> = { Nodes = Map.empty; OutEdges = Map.empty }

    module Node =
        let add (stmt : Stmt<_>) (node : Node<_>) : Node<_> =
            new Node<'a>(node.Body @ [stmt])

        let singleton (stmt : Stmt<_>) : Node<_> =
            new Node<'a>([stmt])

    module Graph =
        let makeNodeId =
            let nextNodeId = ref 1
            fun () -> Interlocked.Increment(nextNodeId)

        let addNode (id : NodeId) (node : Node<_>) (graph : Graph<_>) : Graph<_> =
            { graph with Nodes = Map.add id node graph.Nodes }

        let addEdge (fromId : NodeId) (edge : Edge<'a>) (graph : Graph<_>) : Graph<_> =
            { graph with OutEdges = Map.add fromId edge graph.OutEdges }

        let addIf (ifTrue : 'a) (ifFalse : 'a) (graph : Graph<_>) : NodeId * NodeId * Graph<_> =
            let ifTrueId,  ifTrueNode =  makeNodeId (), Node.singleton (Expr ifTrue)
            let ifFalseId, ifFalseNode = makeNodeId (), Node.singleton (Expr ifFalse)

            let graph =
                graph
                |> addNode ifTrueId ifTrueNode
                |> addNode ifFalseId ifFalseNode

            ifTrueId, ifFalseId, graph

    let graph (block : Block<Expr<_>>) : Graph<Expr<_>> * NodeId list =
        let rec addToGraph
            (body      : Stmt<_> list)
            (entryNode : NodeId option)
            (preds     : NodeId list)
            (graph     : Graph<_>)
                       : NodeId option * NodeId list * Graph<_>
            =
            match body with
            | [] ->
                entryNode, preds, graph

            | head :: tail ->
                let succ, graph =
                    match head with
                    | Expr(ApplyIfFunc(_,  ApplyEqFunc(_, a, b), ifEqual, ifNotEqual)) ->
                        let eq, neq, graph = Graph.addIf ifEqual ifNotEqual graph
                        Choice1Of2 (IfEqual(a, b, eq, neq), [eq; neq]), graph

                    | Expr(ApplyIfFunc(_, test, ifTrue, ifFalse)) ->
                        let t, f, graph = Graph.addIf ifTrue ifFalse graph
                        Choice1Of2 (IfTrue(test, t, f), [t; f]), graph

                    | stmt ->
                        Choice2Of2 stmt, graph
                
                match preds, succ with
                | [], Choice1Of2 (edge, nextPreds) ->
                    let id = Graph.makeNodeId ()

                    graph
                    |> Graph.addNode id Node.Empty
                    |> Graph.addEdge id edge
                    |> addToGraph tail nextPreds

                | [id], Choice1Of2 (edge, nextPreds) ->
                    graph
                    |> Graph.addEdge id edge
                    |> addToGraph tail nextPreds

                | ids, Choice1Of2 (edge, nextPreds) ->
                    let id = Graph.makeNodeId ()
                    let graph = Graph.addNode id Node.Empty graph

                    ids
                    |> List.fold (fun g fromId -> Graph.addEdge fromId (Always id) g) graph
                    |> Graph.addEdge id edge
                    |> addToGraph tail nextPreds

                | [], Choice2Of2 stmt ->
                    let id = Graph.makeNodeId ()

                    graph
                    |> Graph.addNode id (Node.singleton stmt)
                    |> addToGraph tail [id]

                | [id], Choice2Of2 stmt ->
                    graph
                    |> Graph.addNode id (Node.add stmt graph.Nodes.[id])
                    |> addToGraph tail [id]

                | ids, Choice2Of2 stmt ->
                    let id = Graph.makeNodeId ()
                    let graph = Graph.addNode id (Node.singleton stmt) graph

                    ids
                    |> List.fold (fun g fromId -> Graph.addEdge fromId (Always id) g) graph
                    |> addToGraph tail [id]

        addToGraph block.Body [] Graph.Empty