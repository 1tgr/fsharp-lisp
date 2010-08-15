#light
namespace Tim.Lisp.Core
open System
open Tim.Lisp.Core.Syntax

module internal CompilerImpl =
    type InitEnv = Map<string, Expr<unit>>

    type Stmt<'a, 'exprTag> = Scope of 'a * Stmt<'a, 'exprTag> list
                            | Stmt of 'a * Expr<'exprTag>

    let makeScope (exprs : Expr<'a> list) : Stmt<InitEnv, 'a> =
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
                        let oenv, obody = Map.add name value env, []
                        tail |> addToScope (oenv, obody)

                    | _ ->
                        let ienv, ibody = tail |> addToScope (Map.ofList [(name, value)], [])
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
        |> addToScope (Map.empty, List.empty)
        |> Scope

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

    let rec allocateLocals (locals : LocalEnv) (stmt : Stmt<InitEnv, 'a>) : Stmt<LocalEnv, 'a> =
        match stmt with
        | Scope(initEnv, body) ->
            let localEnv = initEnv |> Map.fold addToLocals locals
            let body = body |> List.map (allocateLocals localEnv)
            Scope(localEnv, body)

        | Stmt(_, expr) ->
            Stmt(locals, expr)

    let rec resolveLocal (parentEnvs : LocalEnv list) (expr : Expr<'a>) : Expr<'a> =
        match expr with
        | Atom(a, name) ->
            match parentEnvs with
            | head :: tail ->
                match Map.tryFind name head with
                | Some ref -> Expr.mapa (fun () -> a) ((!ref).InitExpr)
                | None -> resolveLocal tail expr

            | [] ->
                failwithf "%s: undeclared identifier" name

        | List(a, exprs) ->
            let exprs = exprs |> List.map (resolveLocal parentEnvs)
            List(a, exprs)

        | expr ->
            expr

    let rec resolveLocals (parentEnvs : LocalEnv list) (stmt : Stmt<LocalEnv, 'a>) : Stmt<unit, 'a> =
        match stmt with
        | Scope(localEnv, body) ->
            let body = body |> List.map (resolveLocals (localEnv :: parentEnvs))
            Scope((), body)

        | Stmt(localEnv, expr) ->
            Stmt((), resolveLocal parentEnvs expr)

    let rec print (indent : int) (stmt : Stmt<_, _>) : string list =
        let prefix = new string(' ', indent)
        match stmt with
        | Scope(_, body) -> (sprintf "%sScope:" prefix) :: (body |> List.collect (print (indent + 1)))
        | Stmt(_, expr) -> [expr |> sprintf "%s%A" prefix]

module Compiler =
    let compileToDelegate (delegateType : Type) (code : Expr<_> list) : Delegate =
        let msg = 
            code
            |> CompilerImpl.makeScope
            |> CompilerImpl.allocateLocals Map.empty
            |> CompilerImpl.resolveLocals List.empty
            |> CompilerImpl.print 0
            |> String.concat "\n"

        raise <| NotImplementedException ("\n" + msg)
