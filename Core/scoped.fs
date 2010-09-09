#light
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Threading

module Scoped =
    open Syntax

    type DeclId = int

    type Block<'a> =
        {
            Env : Env<'a>
            Body : Stmt<'a> list
        }
        static member empty : Block<'a> = { Env = Env.empty
                                            Body = List.empty }

    and Stmt<'a> = Block of Block<'a>
                 | Expr of 'a

    and Func<'a> =
        {
            Block : Block<'a> ref
            Params : string list
        }

    and Var<'a> =
        {
            DeclEnv : Env<'a>
            InitExpr : 'a
        }

    and EnvValue<'a> = Arg of int
                     | EqFunc
                     | Func of DeclId * Func<'a>
                     | IfFunc
                     | NetFunc of MethodInfo
                     | RecursiveFunc of DeclId * string list
                     | Var of DeclId * Var<'a>

    and Env<'a> =
        {
            Parent : Env<'a> option
            Func : DeclId
            Values : Map<string, EnvValue<'a>>
            Refs : Assembly list
            Using : Set<string>
        }
        static member empty : Env<'a> = { Parent = None
                                          Func = 0
                                          Values = Map.empty
                                          Refs = List.empty
                                          Using = Set.empty }

    let rec foldValue (fn : 'a -> string -> EnvValue<_> -> 'a) (state : 'a) (name : string) (value : EnvValue<_>) : 'a =
        match value with
        | Func(id, func) -> foldEnv fn (fn state name value) (!func.Block).Env
        | _ -> fn state name value

    and foldEnv (fn : 'a -> string -> EnvValue<_> -> 'a) (state : 'a) (env : Env<_>) : 'a =
        Map.fold (foldValue fn) state env.Values

    and foldStmt (fn : 'a -> string -> EnvValue<_> -> 'a) (state : 'a) (stmt : Stmt<_>) : 'a =
        match stmt with
        | Block block -> List.fold (foldStmt fn) (foldEnv fn state block.Env) block.Body
        | _ -> state

    let nextDeclId : unit -> DeclId =
        let id = ref 0
        fun () -> Interlocked.Increment(id)

    let parseRef (values : Expr list) (env : Env<_>) : Env<_> =
        let assembly = 
            match values with
            | [String(_, filename)] ->
                if File.Exists(filename) then
                    Assembly.LoadFrom(filename)
                else
                    Assembly.Load(filename)

            | _ ->
                failwithf ".ref expected 1 value, not %A" values

        { env with Refs = assembly :: env.Refs }

    let parseUsing (values : Expr list) (env : Env<_>) : Env<_> =
        let nspace = 
            match values with
            | [Atom(_, name)] -> name
            | _ -> failwithf ".using expected 1 value, not %A" values

        { env with Using = Set.add nspace env.Using }

    let rec parseDefine (values : Expr list) (env : Env<_>) : Env<_> =
        let name, value = 
            match values with
            | [Atom(_, name); value] ->
                let var = { DeclEnv = env
                            InitExpr = value }

                name, Var(nextDeclId (), var)

            | List(_, Atom(_, name) :: atoms) :: body ->
                let nameOfAtom =
                    function
                    | Atom(_, name) -> name
                    | _ -> failwith "expected atom"

                let paramNames = List.map nameOfAtom atoms
                let id, func = makeFunc env name paramNames body
                name, Func(id, func)

            | _ ->
                failwithf "define expected 1 value, not %A" values

        { env with Values = Map.add name value env.Values }

    and makeFunc (env : Env<_>) (name : string) (paramNames : string list) (body : Expr list) : DeclId * Func<_> =
        let blockRef = ref Block.empty
        let func = { Block = blockRef
                     Params = paramNames }

        let id = nextDeclId()
        let envValues = (name, RecursiveFunc(id, paramNames)) :: List.mapi (fun i name -> name, Arg i) paramNames

        let funcEnv = { env with Parent = Some env
                                 Func = id
                                 Values = Map.ofList envValues }

        blockRef := makeBlock funcEnv body
        id, func

    and makeBlock (parentEnv : Env<Expr>) (exprs : Expr list) : Block<Expr> =
        let rec enterEnv (fn : Env<_> -> Env<_>) (body : Expr list) (block : Block<_>) : Block<_> =
            match block with
            | { Body = [] } ->
                let env = fn block.Env
                addToBlock { block with Env = env } body

            | _ ->
                let env = fn { block.Env with Parent = Some block.Env
                                              Values = Map.empty }

                let iblock = addToBlock { Block.empty with Env = env } body
                { block with Body = block.Body @ [Block iblock] }

        and addToBlock
            (block : Block<_>) 
            (exprs : Expr list) 
                   : Block<_>
            =
            match exprs with
            | List(_, Atom(_, "define") :: values) :: tail -> enterEnv (parseDefine values) tail block
            | List(_, Atom(_, ".ref") :: values) :: tail -> enterEnv (parseRef values) tail block
            | List(_, Atom(_, ".using") :: values) :: tail -> enterEnv (parseUsing values) tail block
            | head :: tail -> addToBlock { block with Body = block.Body @ [Expr head] } tail
            | [] -> block

        addToBlock { Block.empty with Env = parentEnv } exprs
