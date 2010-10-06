#light
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Threading

module Scoped =
    open Syntax

    type DeclId = int
    type EnvId = int

    type Stmt<'a> = Chain of Stmt<'a> * Stmt<'a>
                  | EnterEnv of EnvId * (Stmt<'a> option)
                  | Expr of 'a

    type RecursiveFunc<'ty> =
        {
            Env : EnvId
            Params : (string * 'ty) list
        }

    type Func<'a, 'ty> =
        {
            DeclEnv : EnvId
            Body : Stmt<'a>
            Params : (string * 'ty) list
        }

    type Var<'a> =
        {
            DeclEnv : EnvId
            InitExpr : 'a
        }

    type EnvValue<'a, 'ty> = Arg of int
                           | EqFunc
                           | Func of DeclId * Func<'a, 'ty>
                           | IfFunc
                           | Macro of string list * (Syntax.Expr list)
                           | NetFunc of MethodInfo
                           | RecursiveFunc of DeclId * RecursiveFunc<'ty>
                           | Var of DeclId * Var<'a>

    type Env<'a, 'ty> =
        {
            Parent : EnvId option
            Func : DeclId
            Values : Map<string, EnvValue<'a, 'ty>>
            Refs : Assembly list
            Using : Set<string>
        }
        static member empty : Env<'a, 'ty> = { Parent = None
                                               Func = 0
                                               Values = Map.empty
                                               Refs = List.empty
                                               Using = Set.empty }

    let nextId : unit -> int =
        let id = ref 0
        fun () -> Interlocked.Increment(id)

    let rec tryLookup (envs : Map<EnvId, Env<_, _>>) (name : string) (envId : EnvId) : EnvValue<_, _> option =
        let env = envs.[envId]
        match Map.tryFind name env.Values, env.Parent with
        | Some value, _ -> Some value
        | None, Some parent -> tryLookup envs name parent
        | None, None -> None

    let rec lookup (envs : Map<EnvId, Env<_, _>>) (name : string) (envId : EnvId) : EnvValue<_, _> =
        match tryLookup envs name envId with
        | Some value -> value
        | None -> failwithf "undeclared identifier %s" name

    let rec expandMacroArgs (args : Map<string, Expr>) (expr : Expr) : Expr =
        match expr with
        | Atom(_, name) ->
            match Map.tryFind name args with
            | Some value -> value
            | None -> expr

        | List(a, values) ->
            List(a, List.map (expandMacroArgs args) values)

        | _ ->
            expr

    let rec expandMacros (envs : Map<EnvId, Env<_, _>>) (envId : EnvId) (expr : Expr) : Expr =
        let body =
            match expr with
            | List(_, Atom(_, name) :: values) ->
                match tryLookup envs name envId with
                | Some (Macro(args, body)) ->
                    let map = Map.ofList (args |> List.zip <| values)
                    body
                    |> List.map (expandMacroArgs map)
                    |> Some

                | _ ->
                    None

            | _ -> None

        let pos =
            function
            | Atom(a, _) -> a
            | Float(a, _) -> a
            | Int(a, _) -> a
            | List(a, _) -> a
            | String(a, _) -> a

        match body, expr with
        | Some [body], _ ->
            let body = expandMacros envs envId body
            body

        | Some body, _ ->
            let a = pos expr
            let body = List.map (expandMacros envs envId) body
            List(a, Atom(a, "begin") :: body)

        | None, List(a, values) ->
            List(a, List.map (expandMacros envs envId) values)

        | None, _ ->
            expr

    let parseDefMacro
        (values : Expr list)
        (envs   : Map<EnvId, Env<_, _>> ref)
        (envId  : EnvId)
                : Env<_, string>
        =
        let nameOfAtom =
            function
            | Atom(_, name) -> name
            | a -> failwith "expected atom, not %A" a

        let name, value =
            match values with
            | Atom(_, name) :: body -> name, Macro([], body)
            | List(_, Atom(_, name) :: paramNames) :: body -> name, Macro(List.map nameOfAtom paramNames, body)
            | _ -> failwithf "defmacro expected name, not %A" values

        let env = (!envs).[envId]
        { env with Values = Map.add name value env.Values }

    let parseRef
        (values : Expr list)
        (envs   : Map<EnvId, Env<_, _>> ref)
        (envId  : EnvId)
                : Env<_, string>
        =
        let assembly = 
            match values with
            | [String(_, filename)] ->
                if File.Exists(filename) then
                    Assembly.LoadFrom(filename)
                else
                    Assembly.Load(filename)

            | _ ->
                failwithf ".ref expected 1 value, not %A" values

        let env = (!envs).[envId]
        { env with Refs = assembly :: env.Refs }

    let parseUsing
        (values : Expr list)
        (envs   : Map<EnvId, Env<_, _>> ref)
        (envId  : EnvId)
                : Env<_, string>
        =
        let nspace = 
            match values with
            | [Atom(_, name)] -> name
            | _ -> failwithf ".using expected 1 value, not %A" values

        let env = (!envs).[envId]
        { env with Using = Set.add nspace env.Using }

    let rec parseDefine
        (values : Expr list)
        (envs   : Map<EnvId, Env<_, _>> ref)
        (envId  : EnvId)
                : Env<_, string>
        =
        let name, value = 
            match values with
            | [Atom(_, name); value] ->
                let var = { DeclEnv = envId
                            InitExpr = value }

                name, Var(nextId (), var)

            | List(_, Atom(_, name) :: atoms) :: body ->
                let nameOfAtom =
                    function
                    | Atom(_, name) -> name, "System.Int32"
                    | List(_, [Atom(_, name); Atom(_, typeName)]) -> name, typeName
                    | _ -> failwith "expected atom"

                let paramNames = List.map nameOfAtom atoms
                name, makeFunc envs envId name paramNames body

            | _ ->
                failwithf "define expected 1 value, not %A" values

        let env = (!envs).[envId]
        { env with Values = Map.add name value env.Values }

    and tryParseSpecial (expr : Expr) : (Map<EnvId, Env<_, _>> ref -> EnvId -> Env<_, string>) option =
        match expr with
        | List(_, Atom(_, "define") :: values) -> Some (parseDefine values)
        | List(_, Atom(_, "defmacro") :: values) -> Some (parseDefMacro values)
        | List(_, Atom(_, ".ref") :: values) -> Some (parseRef values)
        | List(_, Atom(_, ".using") :: values) -> Some (parseUsing values)
        | _ -> None

    and makeFunc
        (envs  : Map<EnvId, Env<_, _>> ref)
        (envId : EnvId)
        (name  : string)
        (parms : (string * string) list)
        (body  : Expr list)
               : EnvValue<_, string>
        =
        let funcId = nextId ()
        let funcEnvId = nextId ()

        let recursiveFunc = { Env = funcEnvId
                              Params = parms }

        let envValues = (name, RecursiveFunc(funcId, recursiveFunc)) :: 
                        List.mapi (fun i (name, _) -> name, Arg i) parms

        let funcEnv = { (!envs).[envId] with Parent = Some envId
                                             Func = funcId
                                             Values = Map.ofList envValues }

        envs := Map.add funcEnvId funcEnv !envs

        let innerBody = scopedExpr envs funcEnvId body
        Func(funcId, { DeclEnv = envId
                       Body = EnterEnv(funcEnvId, innerBody)
                       Params = parms })

    and scopedExpr
        (envs       : Map<EnvId, Env<_, _>> ref)
        (outerEnvId : EnvId)
        (exprs      : Expr list)
                    : Stmt<_> option
        =
        match exprs with
        | head :: tail ->
            let head = expandMacros !envs outerEnvId head
            match tryParseSpecial head with
            | Some fn ->
                let innerEnvId = nextId ()

                let env = { (!envs).[outerEnvId] with Parent = Some outerEnvId
                                                               Values = Map.empty }
                envs := Map.add innerEnvId env !envs

                let env = fn envs innerEnvId
                envs := Map.add innerEnvId env !envs

                Some (EnterEnv(innerEnvId, scopedExpr envs innerEnvId tail))

            | None ->
                let stmt =
                    match head with
                    | List(_, Atom(_, "begin") :: body) -> scopedExpr envs outerEnvId body
                    | expr -> Some (Expr expr)

                match stmt, scopedExpr envs outerEnvId tail with
                | Some first, Some second -> Some (Chain(first, second))
                | None, Some second -> Some second
                | Some first, None -> Some first
                | None, None -> None

        | [] -> None

    let getType (env : Env<_, _>) (name : string) : Type =
        let inAssembly (ref : Assembly) : Type option = 
            env.Using
            |> Seq.tryPick (fun nspace ->
                match ref.GetType(nspace + "." + name, false) with
                | null -> None
                | t -> Some t)

        match List.tryPick inAssembly env.Refs with
        | Some t -> t
        | None -> failwithf "%s is not a .NET type" name
