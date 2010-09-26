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

    let rec stmtType (stmt : Stmt<Expr>) : Type =
        match stmt with
        | Chain(_, b) -> stmtType b
        | EnterEnv(_, Some stmt) -> stmtType stmt
        | EnterEnv(_, None) -> typeof<Void>
        | Expr expr -> exprType expr

    let typedRecursiveFunc (envs : Map<EnvId, Env<Syntax.Expr, string>>) (func : RecursiveFunc<string>) : RecursiveFunc<Type> =
        let env = envs.[func.Env]
        { Env = func.Env
          Params = List.map (fun (name, ty) -> (name, getType env ty)) func.Params }

    let rec typedAsm (envs : Map<EnvId, Env<Syntax.Expr, string>>) (envId : EnvId) (asm : Asm<Syntax.Expr>) : Asm<Expr> =
        { OpCode = asm.OpCode
          Operand = asm.Operand
          ResultType = asm.ResultType
          Stack = List.map (typedExpr envs envId) asm.Stack }
    
    and typedFunc (envs : Map<EnvId, Env<Syntax.Expr, string>>) (func : Func<Syntax.Expr, string>) : Func<Expr, Type> =
        let env = envs.[func.DeclEnv]
        { DeclEnv = func.DeclEnv
          Body = typedStmt envs func.DeclEnv func.Body
          Params = List.map (fun (name, ty) -> (name, getType env ty)) func.Params }

    and typedVar (envs : Map<EnvId, Env<Syntax.Expr, string>>) (var : Var<Syntax.Expr>) : Var<Expr> =
        { DeclEnv = var.DeclEnv
          InitExpr = typedExpr envs var.DeclEnv var.InitExpr }

    and typedStmt (envs : Map<EnvId, Env<Syntax.Expr, string>>) (envId : EnvId) (stmt : Stmt<Syntax.Expr>) : Stmt<Expr> =
        match stmt with
        | Chain(a, b)-> Chain(typedStmt envs envId a, typedStmt envs envId b)
        | EnterEnv(envId, stmt) -> EnterEnv(envId, Option.map (typedStmt envs envId) stmt)
        | Expr expr -> Expr (typedExpr envs envId expr)

    and typedExpr (envs : Map<EnvId, Env<Syntax.Expr, string>>) (envId : EnvId) (expr : Syntax.Expr) : Expr =
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
            match lookup envs name envId with
            | Arg n -> LookupArg(a, typeof<int>, n)
            | Var(id, var) -> LookupVar(a, exprType ((typedVar envs var).InitExpr), id)
            | _ ->  failwith "delegates not yet implemented"

        | Syntax.Float(a, n) -> Float(a, n)
        | Syntax.Int(a, n) -> Int(a, n)

        | Syntax.List(a, values) ->
            let env = envs.[envId]
            match tryParseAsm env expr, values with
            | Some asm, _ ->
                Asm(a, typedAsm envs envId asm)

            | None, Syntax.Atom(_, name) :: args ->
                match lookup envs name envId with
                | EqFunc ->
                    match args with
                    | [x; y] ->
                        ApplyEqFunc(a, typedExpr envs envId x, typedExpr envs envId y)

                    | _ ->
                        failwith "expected 2 args for =, not %A" args

                | Func(id, func) ->
                    ApplyFunc(a, stmtType (typedStmt envs func.DeclEnv func.Body), id, List.map (typedExpr envs envId) args)

                | IfFunc ->
                    match args with
                    | [test; ifTrue; ifFalse] ->
                        ApplyIfFunc(a, typedExpr envs envId test, typedExpr envs envId ifTrue, typedExpr envs envId ifFalse)

                    | _ ->
                        failwith "expected 3 args for if, not %A" args

                | Macro _ ->
                    failwithf "didn't expect macro"

                | NetFunc mi ->
                    ApplyNetFunc(a, mi, List.map (typedExpr envs envId) args)

                | RecursiveFunc(id, _) ->
                    // TODO type inference for recursive functions
                    ApplyFunc(a, typeof<int>, id, List.map (typedExpr envs envId) args)

                | Arg _ | Var _ ->
                    failwith "delegates not yet implemented"

            | None, _ ->
                failwith "quotation not yet implemented"

        | Syntax.String(a, s) -> String(a, s)

    let typedValue (envs : Map<EnvId, Env<Syntax.Expr, string>>) (value : EnvValue<Syntax.Expr, string>) : EnvValue<Expr, Type> =
        match value with
        | Arg n -> Arg n
        | EqFunc -> EqFunc
        | Func(id, func) -> Func(id, typedFunc envs func)
        | IfFunc -> IfFunc
        | Macro body -> Macro body
        | NetFunc mi -> NetFunc mi
        | RecursiveFunc(id, func) -> RecursiveFunc(id, typedRecursiveFunc envs func)
        | Var(id, var) -> Var(id, typedVar envs var)

    let typedEnv (envs : Map<EnvId, Env<Syntax.Expr, string>>) (env : Env<Syntax.Expr, string>) : Env<Expr, Type> =
        { Parent = env.Parent
          Func = env.Func
          Values = Map.map (fun _ -> typedValue envs) env.Values
          Refs = env.Refs
          Using = env.Using }
