#light
namespace Tim.Lisp.Core
open System
open Tim.Lisp.Core.Syntax

module internal List =
    let singleton value =
        [value]

module internal CompilerImpl =
    type Env = Map<string, Expr<unit>>

    let rec addEnvironments (env : Env) (exprs : Expr<'a> list) : Expr<'a * Env> list =
        match exprs with
        | List(_, Atom(_, "define") :: values) :: tail ->
            match values with
            | [Atom(_, name); value] ->
                let env = env |> Map.add name (Expr.mapa (fun _ -> ()) value)
                addEnvironments env tail

            | List _ :: _ ->
                failwithf "Functions not yet implemented"

            | _ ->
                failwithf "define expected 1 value, not %A" values

        | head :: tail ->
            let expr =
                match head with
                | List(a, exprs) ->
                    let exprs = exprs |> List.map (List.singleton >> addEnvironments env >> List.head)
                    List((a, env), exprs)

                | expr ->
                    expr |> Expr.mapa (fun a -> (a, env))

            expr :: addEnvironments env tail

        | [] -> []

    type Local =
        {
            Name : string
            InitExpr : Expr<unit>
        }

    type LocalEnv = Map<string, Local ref>

    let addToLocals (locals : LocalEnv) (name : string) (initExpr : Expr<unit>) : LocalEnv =
        match Map.tryFind name locals with
        | Some ref when (!ref).InitExpr = initExpr ->
            locals

        | _ ->
            let local = { Name = name
                          InitExpr = initExpr }
            Map.add name (ref local) locals

    let rec allocateLocals (locals : LocalEnv) (exprs : Expr<'a * Env> list) : Expr<'a * LocalEnv> list =
        match exprs with
        | head :: tail ->
            let expr =
                match head with
                | List((a, env), exprs) ->
                    let locals = env |> Map.fold addToLocals locals
                    let exprs = exprs |> List.map (List.singleton >> allocateLocals locals >> List.head)
                    List((a, locals), exprs)

                | expr ->
                    expr |> Expr.mapa (fun (a, env) -> a, env |> Map.fold addToLocals locals)

            expr :: allocateLocals (expr |> Expr.annot |> snd) tail

        | [] -> []

    let rec resolveLocals (expr : Expr<'a * LocalEnv>) : Expr<unit> =
        match expr with
        | Atom ((_, locals), name) ->
            match Map.tryFind name locals with
            | Some ref -> (!ref).InitExpr
            | None -> failwithf "%s wasn't declared" name
        
        | List((a, locals), exprs) ->
            let exprs = exprs |> List.map resolveLocals
            List((), exprs)

        | expr ->
            expr |> Expr.mapa (fun _ -> ())

module Compiler =
    let compileToDelegate (delegateType : Type) (code : Expr<_> list) : Delegate =
        let msg = 
            code
            |> CompilerImpl.addEnvironments Map.empty
            |> CompilerImpl.allocateLocals Map.empty
            |> List.map CompilerImpl.resolveLocals
            |> sprintf "%A"

        raise <| NotImplementedException msg
