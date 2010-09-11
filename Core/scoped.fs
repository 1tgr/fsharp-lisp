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

    type Block<'a> =
        {
            Env : EnvId
            Body : Stmt<'a> list
        }

    and Stmt<'a> = Block of Block<'a>
                 | Expr of 'a

    and Func<'a, 'ty> =
        {
            Block : Block<'a>
            Params : (string * 'ty) list
        }

    and Var<'a> =
        {
            DeclEnv : EnvId
            InitExpr : 'a
        }

    and EnvValue<'a, 'ty> = Arg of int
                          | EqFunc
                          | Func of DeclId * Func<'a, 'ty>
                          | IfFunc
                          | NetFunc of MethodInfo
                          | RecursiveFunc of DeclId * (string * 'ty) list
                          | Var of DeclId * Var<'a>

    and Env<'a, 'ty> =
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

    let rec foldValue (envs : Map<EnvId, Env<_, _>>) (fn : 'a -> string -> EnvValue<_, _> -> 'a) (state : 'a) (name : string) (value : EnvValue<_, _>) : 'a =
        match value with
        | Func(id, func) -> foldEnv envs fn (fn state name value) func.Block.Env
        | _ -> fn state name value

    and foldEnv (envs : Map<EnvId, Env<_, _>>) (fn : 'a -> string -> EnvValue<_, _> -> 'a) (state : 'a) (envId : EnvId) : 'a =
        Map.fold (foldValue envs fn) state (envs.[envId].Values)

    and foldStmt (envs : Map<EnvId, Env<_, _>>) (fn : 'a -> string -> EnvValue<_, _> -> 'a) (state : 'a) (stmt : Stmt<_>) : 'a =
        match stmt with
        | Block block -> List.fold (foldStmt envs fn) (foldEnv envs fn state block.Env) block.Body
        | _ -> state

    let nextId : unit -> int =
        let id = ref 0
        fun () -> Interlocked.Increment(id)

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
                let funcId, func = makeFunc envs envId name paramNames body
                name, Func(funcId, func)

            | _ ->
                failwithf "define expected 1 value, not %A" values

        let env = (!envs).[envId]
        { env with Values = Map.add name value env.Values }

    and makeFunc
        (envs       : Map<EnvId, Env<_, _>> ref)
        (envId      : EnvId)
        (name       : string)
        (paramNames : (string * string) list)
        (body       : Expr list)
                    : DeclId * Func<_, string>
        =
        let funcId = nextId ()
        let envValues = (name, RecursiveFunc(funcId, paramNames)) :: List.mapi (fun i (name, _) -> name, Arg i) paramNames

        let funcEnv = { (!envs).[envId] with Parent = Some envId
                                             Func = funcId
                                             Values = Map.ofList envValues }

        let funcEnvId = nextId ()
        envs := Map.add funcEnvId funcEnv !envs
        let block = makeBlock envs funcEnvId body
        funcId, { Block = block; Params = paramNames }

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
            envs := Map.add block.Env env !envs

            let env = fn envs envId
            envs := Map.add block.Env env !envs

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
        | List(_, Atom(_, ".ref") :: values) :: tail -> enterEnv envs (parseRef values) tail block
        | List(_, Atom(_, ".using") :: values) :: tail -> enterEnv envs (parseUsing values) tail block

        | head :: tail -> addToBlock envs
                                     { block with Body = block.Body @ [Expr head] }
                                     tail

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
