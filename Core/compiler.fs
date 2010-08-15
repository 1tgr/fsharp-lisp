#light
namespace Tim.Lisp.Core
open System
open Tim.Lisp.Core.Syntax

module internal List =
    let singleton value =
        [value]

module internal CompilerImpl =
    type Block<'a, 'b> =
        {
            Env : Map<string, Expr<unit>>
            Body : Stmt<'a, 'b> list
        }

    and Stmt<'a, 'b> = Scope of 'a * Block<'a, 'b>
                     | Stmt of 'a * Expr<'b>

    let makeScope (exprs : Expr<'a> list) : Stmt<unit, 'a> =
        let rec append (appendTo : Block<unit, 'a>) (exprs : Expr<'a> list) : Block<unit, 'a> =
            match exprs with
            | List(_, Atom(_, "define") :: values) :: tail ->
                match values with
                | [Atom(_, name); value] ->
                    let value = value |> Expr.mapa (fun _ -> ())

                    match appendTo with
                    | { Env = env; Body = [] } ->
                        append { appendTo with Env = Map.add name value env } tail

                    | { Body = body } ->
                        let block = { Env = Map.ofList [(name, value)]
                                      Body = [] }

                        { appendTo with Body = body @ [Scope((), append block tail)] }

                | List _ :: _ ->
                    failwithf "Functions not yet implemented"

                | _ ->
                    failwithf "define expected 1 value, not %A" values

            | head :: tail ->
                append { appendTo with Body = appendTo.Body @ [Stmt((), head)] } tail

            | [] -> appendTo

        Scope((), append { Env = Map.empty; Body = List.empty } exprs)

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

    let rec allocateLocals (locals : LocalEnv) (stmt : Stmt<unit, 'a>) : Stmt<LocalEnv, 'a> =
        match stmt with
        | Scope((), { Env = env; Body = body }) ->
            let block = { Env = env
                          Body = List.map (allocateLocals locals) body }

            let env = env |> Map.fold addToLocals locals
            Scope(env, block)

        | Stmt((), expr) ->
            Stmt(locals, expr)

    let resolveLocal (expr : Expr<'a>) (env : LocalEnv) : Expr<unit> option =
        match expr with
        | Atom(_, name) ->
            match Map.tryFind name env with
            | Some ref -> Some ((!ref).InitExpr)
            | None -> None

        | expr ->
            Some (expr |> Expr.mapa (fun _ -> ()))

    let rec resolveLocals (parentEnvs : LocalEnv list) (stmt : Stmt<LocalEnv, 'a>) : Stmt<unit, unit> =
        match stmt with
        | Scope(localEnv, { Env = env; Body = body }) ->
            let localEnv = localEnv :: parentEnvs
            let block = { Env = env
                          Body = List.map (resolveLocals localEnv) body }
            Scope((), block)

        | Stmt(localEnv, expr) ->
            match List.tryPick (resolveLocal expr) parentEnvs with
            | Some expr -> Stmt((), expr)
            | None -> failwithf "Undeclared identifier in %A" expr

module Compiler =
    let compileToDelegate (delegateType : Type) (code : Expr<_> list) : Delegate =
        let msg = 
            code
            |> CompilerImpl.makeScope
            |> CompilerImpl.allocateLocals Map.empty
            |> CompilerImpl.resolveLocals List.empty
            |> sprintf "%A"

        raise <| NotImplementedException msg
