#light
namespace Tim.Lisp.Core
open System
open System.Reflection
open FParsec

module Typed =
    open Asm
    open Scoped

    type Expr = ApplyEqFunc of Position * Expr * Expr
              | ApplyFunc of Position * Type * DeclId * Expr list
              | ApplyIfFunc of Position * Expr * Expr * Expr
              | ApplyNetFunc of Position * MethodInfo * Expr list
              | Asm of Position * Asm<Expr>
              | Bool of Position * bool
              | Char of Position * char
              | Float of Position * float
              | Int of Position * int
              | LookupArg of Position * Type * int
              | LookupVar of Position * Type * DeclId
              | String of Position * string

    let rec exprType (expr : Expr) : Type =
        match expr with
        | ApplyEqFunc _ -> typeof<bool>
        | ApplyFunc(_, t, _, _) -> t
        | ApplyIfFunc(_, _, ifTrue, _) -> exprType ifTrue
        | ApplyNetFunc(_, mi, _) -> mi.ReturnType
        | Asm(_, a) -> a.ResultType
        | Bool _ -> typeof<bool>
        | Char _ -> typeof<char>
        | Float _ -> typeof<float>
        | Int _ -> typeof<int>
        | LookupArg(_, t, _) -> t
        | LookupVar(_, t, _) -> t
        | String(_, s) -> typeof<string>

    let rec blockType (block : Block<Expr>) : Type =
        match List.rev block.Body with
        | [] -> typeof<Void>
        | Block block :: _ -> blockType block
        | Expr expr :: _ -> exprType expr

    let rec typedAsm (env : Env<Syntax.Expr>) (asm : Asm<Syntax.Expr>) : Asm<Expr> =
        { OpCode = asm.OpCode
          Operand = asm.Operand
          ResultType = asm.ResultType
          Stack = List.map (typedExpr env) asm.Stack }
    
    and typedFunc (func : Func<Syntax.Expr>) : Func<Expr> =
        { Block = typedBlock func.Block
          Params = func.Params }

    and typedVar (var : Var<Syntax.Expr>) : Var<Expr> =
        { DeclEnv = typedEnv var.DeclEnv
          InitExpr = typedExpr var.DeclEnv var.InitExpr }

    and typedValue (value : EnvValue<Syntax.Expr>) : EnvValue<Expr> option =
        match value with
        | Arg n -> Some (Arg n)
        | EqFunc -> Some EqFunc
        | Func(id, func) -> Some (Func(id, typedFunc func))
        | IfFunc -> Some IfFunc
        | NetFunc mi -> Some (NetFunc mi)
        | Macro func -> None
        | MacroArg n -> None
        | RecursiveFunc(id, paramNames) -> Some (RecursiveFunc(id, paramNames))
        | Var(id, var) -> Some (Var(id, typedVar var))

    and typedEnv (env : Env<Syntax.Expr>) : Env<Expr> =
        let values =
            env.Values
            |> Map.toSeq
            |> Seq.choose (fun (name, value) -> 
                match typedValue value with
                | Some value -> Some (name, value)
                | None -> None)
            |> Map.ofSeq

        { Parent = Option.map typedEnv env.Parent
          Func = env.Func
          Values = values
          Refs = env.Refs
          Using = env.Using
          MacroArgs = Array.map (typedExpr env) env.MacroArgs }

    and typedStmt (env : Env<Syntax.Expr>) (stmt : Stmt<Syntax.Expr>) : Stmt<Expr> =
        match stmt with
        | Block block -> Block (typedBlock block)
        | Expr expr -> Expr (typedExpr env expr)

    and typedBlock (block : Block<Syntax.Expr>) : Block<Expr> =
        { Env = typedEnv block.Env
          Body = List.map (typedStmt block.Env) block.Body }

    and typedExpr (env : Env<Syntax.Expr>) (expr : Syntax.Expr) : Expr =
        match expr with
        | Syntax.Atom(a, "#t") ->
            Bool(a, true)

        | Syntax.Atom(a, "#f") ->
            Bool(a, false)

        | Syntax.Atom(a, "#\space") ->
            Char(a, ' ')

        | Syntax.Atom(a, "#\newline") ->
            Char(a, '\n')

        | Syntax.Atom(a, name) when name.Length = 3 && name.StartsWith("#\\") ->
            Char(a, name.[2])

        | Syntax.Atom(a, name) ->
            match lookup name env with
            | Arg n -> LookupArg(a, typeof<int>, n)
            | Var(id, var) -> LookupVar(a, exprType ((typedVar var).InitExpr), id)
            | _ ->  failwith "delegates not yet implemented"

        | Syntax.Float(a, n) -> Float(a, n)
        | Syntax.Int(a, n) -> Int(a, n)

        | Syntax.List(a, values) ->
            match tryParseAsm env.Refs env.Using expr, values with
            | Some asm, _ ->
                Asm(a, typedAsm env asm)

            | None, Syntax.Atom(_, name) :: args ->
                match lookup name env with
                | EqFunc ->
                    match args with
                    | [x; y] ->
                        ApplyEqFunc(a, typedExpr env x, typedExpr env y)

                    | _ ->
                        failwith "expected 2 args for =, not %A" args

                | Func(id, func) ->
                    ApplyFunc(a, blockType (typedBlock func.Block), id, List.map (typedExpr env) args)

                | IfFunc ->
                    match args with
                    | [test; ifTrue; ifFalse] ->
                        ApplyIfFunc(a, typedExpr env test, typedExpr env ifTrue, typedExpr env ifFalse)

                    | _ ->
                        failwith "expected 3 args for if, not %A" args

                | NetFunc mi ->
                    ApplyNetFunc(a, mi, List.map (typedExpr env) args)

                | RecursiveFunc(id, _) ->
                    // TODO type inference for recursive functions
                    ApplyFunc(a, typeof<int>, id, List.map (typedExpr env) args)

                | Arg _ | Var _ ->
                    failwith "delegates not yet implemented"

            | None, _ ->
                failwith "quotation not yet implemented"

        | Syntax.String(a, s) -> String(a, s)
