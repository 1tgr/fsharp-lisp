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

    type RecursiveFunc<'ty> =
        {
            Env : EnvId
            Params : (string * 'ty) list
        }

    type Block<'a> =
        {
            Env : EnvId
            Body : Stmt<'a> list
        }

    and Stmt<'a> = Block of Block<'a>
                 | Expr of 'a

    type Func<'a, 'ty> =
        {
            Block : Block<'a>
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
                           | Macro of Stmt<Syntax.Expr> list
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

    let parseDefMacro
        (values : Expr list)
        (envs   : Map<EnvId, Env<_, _>> ref)
        (envId  : EnvId)
                : Env<_, string>
        =
        let name, value =
            match values with
            | Atom(_, name) :: body -> name, Macro (List.map Expr body)
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
        let block = makeBlock envs funcEnvId body
        Func(funcId, { Block = block; Params = parms })

    and enterEnv
        (envs  : Map<EnvId, Env<_, _>> ref)
        (fn    : Map<EnvId, Env<_, _>> ref -> EnvId -> Env<_, _>)
        (body  : Expr list)
        (block : Block<_>)
               : Block<_>
        =
        match block with
        | { Body = [] } ->
            let env = fn envs block.Env
            envs := Map.add block.Env env !envs
            addToBlock envs block body

        | _ ->
            let envId = nextId ()

            let env = { (!envs).[block.Env] with Parent = Some block.Env
                                                 Values = Map.empty }
            envs := Map.add envId env !envs

            let env = fn envs envId
            envs := Map.add envId env !envs

            let iblock = addToBlock envs
                                    { Env = envId; Body = List.empty }
                                    body

            { block with Body = block.Body @ [Block iblock] }

    and addToBlock
        (envs   : Map<EnvId, Env<_, _>> ref)
        (block  : Block<_>) 
        (exprs  : Expr list) 
                : Block<_>
        =
        match exprs with
        | List(_, Atom(_, "define") :: values) :: tail -> enterEnv envs (parseDefine values) tail block
        | List(_, Atom(_, "defmacro") :: values) :: tail -> enterEnv envs (parseDefMacro values) tail block
        | List(_, Atom(_, ".ref") :: values) :: tail -> enterEnv envs (parseRef values) tail block
        | List(_, Atom(_, ".using") :: values) :: tail -> enterEnv envs (parseUsing values) tail block

        | head :: tail ->
            let body =
                match head with
                | List(_, Atom(_, name) :: values) ->
                    match tryLookup !envs name block.Env with
                    | Some (Macro body) -> body
                    | _ -> [Expr head]

                | _ -> [Expr head]

            addToBlock envs { block with Body = block.Body @ body } tail

        | [] -> block

    and makeBlock
        (envs      : Map<EnvId, Env<_, _>> ref)
        (parentEnv : EnvId)
        (exprs     : Expr list)
                   : Block<Expr>
        =
        addToBlock envs
                   { Env = parentEnv; Body = List.empty }
                   exprs

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
