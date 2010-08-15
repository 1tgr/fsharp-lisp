#light
namespace Tim.Lisp.Core
open System
open Tim.Lisp.Core.Syntax

module internal CompilerImpl =
    type InitEnv =
        {
            Parent : InitEnv option
            Values : Map<string, Expr<unit>>
        }

    type Stmt<'a, 'exprTag> = Scope of 'a * Stmt<'a, 'exprTag> list
                            | Stmt of 'a * Expr<'exprTag>

    let makeScope (env : InitEnv) (exprs : Expr<'a> list) : Stmt<InitEnv, 'a> =
        let rec addToScope
            (env   : InitEnv, body : Stmt<InitEnv, 'a> list) 
            (exprs : Expr<'a> list) 
                   : InitEnv * Stmt<InitEnv, 'a> list
            =
            match exprs with
            | List(_, Atom(_, "define") :: values) :: tail ->
                match values with
                | [Atom(_, name); value] ->
                    let value = value |> Expr.mapa (fun _ -> ())

                    match body with
                    | [] ->
                        tail |> addToScope ({ env with Values = Map.add name value env.Values }, [])

                    | _ ->
                        let ienv = { Parent = Some env
                                     Values = Map.ofList [(name, value)] }

                        let ienv, ibody = tail |> addToScope (ienv, [])
                        env, body @ [Scope(ienv, ibody)]

                | List _ :: _ ->
                    failwithf "Functions not yet implemented"

                | _ ->
                    failwithf "define expected 1 value, not %A" values

            | head :: tail ->
                let scope = env, body @ [Stmt(env, head)]
                tail |> addToScope scope

            | [] ->
                env, body

        exprs
        |> addToScope (env, List.empty)
        |> Scope

    let rec resolveLocal (env : InitEnv) (expr : Expr<'a>) : Expr<'a> =
        match expr with
        | Atom(a, name) ->            
            match Map.tryFind name env.Values with
            | Some initExpr -> Expr.mapa (fun () -> a) initExpr
            | None ->
                match env.Parent with
                | Some env -> resolveLocal env expr
                | None -> failwithf "%s: undeclared identifier" name

        | List(a, exprs) ->
            let exprs = exprs |> List.map (resolveLocal env)
            List(a, exprs)

        | expr ->
            expr

    let rec resolveLocals (stmt : Stmt<InitEnv, 'a>) : Stmt<unit, 'a> =
        match stmt with
        | Scope(_, body) ->
            let body = body |> List.map resolveLocals
            Scope((), body)

        | Stmt(env, expr) ->
            Stmt((), resolveLocal env expr)

    let rec print (indent : int) (stmt : Stmt<_, _>) : string list =
        let prefix = new string(' ', indent)
        match stmt with
        | Scope(_, body) -> (sprintf "%sScope:" prefix) :: (body |> List.collect (print (indent + 1)))
        | Stmt(_, expr) -> [expr |> sprintf "%s%A" prefix]

module Compiler =
    let compileToDelegate (delegateType : Type) (code : Expr<_> list) : Delegate =
        let msg = 
            code
            |> CompilerImpl.makeScope { Parent = None; Values = Map.empty }
            |> CompilerImpl.resolveLocals
            |> CompilerImpl.print 0
            |> String.concat "\n"

        raise <| NotImplementedException ("\n" + msg)
