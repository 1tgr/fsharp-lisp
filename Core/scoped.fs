#light
namespace Tim.Lisp.Core
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
        static member empty : Block<'a> = { Env = Env.empty; Body = List.empty }

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
                     | Func of DeclId * Func<'a>
                     | IfFunc
                     | NetFunc of MethodInfo
                     | Var of DeclId * Var<'a>

    and Env<'a> =
        {
            Parent : Env<'a> option
            Func : DeclId
            Values : Map<string, EnvValue<'a>>
        }
        static member empty : Env<'a> = { Parent = None; Func = 0; Values = Map.empty }

    let nextDeclId : unit -> DeclId =
        let id = ref 0
        fun () -> Interlocked.Increment(id)

    let rec makeFunc (env : Env<_>) (name : string) (paramNames : string list) (body : Expr<_> list) : DeclId * Func<_> =
        let blockRef = ref Block.empty
        let func = { Block = blockRef
                     Params = paramNames }

        let id = nextDeclId()
        let envValues = (name, Func(id, func)) :: List.mapi (fun i name -> name, Arg i) paramNames

        let funcEnv = { Parent = Some env
                        Func = id
                        Values = Map.ofList envValues }

        blockRef := makeBlock funcEnv body
        id, func

    and makeBlock (parentEnv : Env<Expr<'a>>) (exprs : Expr<'a> list) : Block<Expr<'a>> =
        let rec addToBlock
            (block : Block<_>) 
            (exprs : Expr<_> list) 
                   : Block<_>
            =
            match exprs with
            | List(_, Atom(_, "define") :: values) :: tail ->
                let name, value = 
                    match values with
                    | [Atom(_, name); value] ->
                        let var = { DeclEnv = block.Env
                                    InitExpr = value }

                        name, Var(nextDeclId (), var)

                    | List(_, Atom(_, name) :: atoms) :: body ->
                        let nameOfAtom =
                            function
                            | Atom(_, name) -> name
                            | _ -> failwith "expected atom"

                        let paramNames = List.map nameOfAtom atoms
                        let id, func = makeFunc block.Env name paramNames body
                        name, Func(id, func)

                    | _ ->
                        failwithf "define expected 1 value, not %A" values

                match block with
                | { Body = [] } ->
                    let env = block.Env
                    let env = { env with Values = Map.add name value env.Values }
                    tail |> addToBlock { block with Env = env }

                | _ ->
                    let ienv = { block.Env with Parent = Some block.Env
                                                Values = Map.ofList [(name, value)] }

                    let iblock = tail |> addToBlock { Block.empty with Env = ienv }
                    { block with Body = block.Body @ [Block iblock] }

            | head :: tail ->
                tail |> addToBlock { block with Body = block.Body @ [Expr head] }

            | [] ->
                block

        addToBlock { Block.empty with Env = parentEnv } exprs
