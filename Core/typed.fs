#light
namespace Tim.Lisp.Core
open System
open System.Reflection

module Typed =
    open Asm
    open Scoped

    type Expr<'a> = ApplyEqFunc of 'a * Expr<'a> * Expr<'a>
                  | ApplyFunc of 'a * Type * DeclId * Expr<'a> list
                  | ApplyIfFunc of 'a * Expr<'a> * Expr<'a> * Expr<'a>
                  | ApplyNetFunc of 'a * MethodInfo * Expr<'a> list
                  | Asm of 'a * Asm<Expr<'a>>
                  | Float of 'a * float
                  | Int of 'a * int
                  | LookupArg of 'a * Type * int
                  | LookupVar of 'a * Type * DeclId
                  | String of 'a * string

    let rec lookup (name : string) (env : Env<_>) : EnvValue<_> =
        match Map.tryFind name env.Values, env.Parent with
        | Some value, _ -> value
        | None, Some parent -> lookup name parent
        | None, None -> failwithf "undeclared identifier %s" name

    let rec exprType (expr : Expr<'a>) : Type =
        match expr with
        | ApplyEqFunc _ -> typeof<bool>
        | ApplyFunc(_, t, _, _) -> t
        | ApplyIfFunc(_, _, ifTrue, _) -> exprType ifTrue
        | ApplyNetFunc(_, mi, _) -> mi.ReturnType
        | Asm(_, a) -> a.ResultType
        | Float _ -> typeof<float>
        | Int _ -> typeof<int>
        | LookupArg(_, t, _) -> t
        | LookupVar(_, t, _) -> t
        | String(_, s) -> typeof<string>

    let rec blockType (block : Block<Expr<_>>) : Type =
        match List.rev block.Body with
        | [] -> typeof<Void>
        | Block block :: _ -> blockType block
        | Expr expr :: _ -> exprType expr

    let rec typedAsm (env : Env<Syntax.Expr<_>>) (asm : Asm<Syntax.Expr<_>>) : Asm<Expr<_>> =
        { OpCode = asm.OpCode
          Operand = asm.Operand
          ResultType = asm.ResultType
          Stack = List.map (typedExpr env) asm.Stack }
    
    and typedFunc (func : Func<Syntax.Expr<_>>) : Func<Expr<_>> =
        { Block = ref (typedBlock !func.Block)
          Params = func.Params }  

    and typedVar (var : Var<Syntax.Expr<_>>) : Var<Expr<_>> =
        { DeclEnv = typedEnv var.DeclEnv
          InitExpr = typedExpr var.DeclEnv var.InitExpr }

    and typedValue (value : EnvValue<Syntax.Expr<_>>) : EnvValue<Expr<_>> =
        match value with
        | Arg n -> Arg n
        | EqFunc -> EqFunc
        | Func(id, func) -> Func(id, typedFunc func)
        | IfFunc -> IfFunc
        | NetFunc mi -> NetFunc mi
        | RecursiveFunc(id, paramNames) -> RecursiveFunc(id, paramNames)
        | Var(id, var) -> Var(id, typedVar var)

    and typedEnv (env : Env<Syntax.Expr<_>>) : Env<Expr<_>> =
        { Parent = Option.map typedEnv env.Parent
          Func = env.Func
          Values = Map.map (fun _ -> typedValue) env.Values }

    and typedStmt (env : Env<Syntax.Expr<_>>) (stmt : Stmt<Syntax.Expr<_>>) : Stmt<_> =
        match stmt with
        | Block block -> Block (typedBlock block)
        | Expr expr -> Expr (typedExpr env expr)

    and typedBlock (block : Block<Syntax.Expr<_>>) : Block<Expr<_>> =
        { Env = typedEnv block.Env
          Body = List.map (typedStmt block.Env) block.Body }

    and typedExpr (env : Env<Syntax.Expr<_>>) (expr : Syntax.Expr<_>) : Expr<_> =
        match expr with
        | Syntax.Atom(a, name) ->
            match lookup name env with
            | Arg n ->
                LookupArg(a, typeof<int>, n)

            | Var(id, var) ->
                LookupVar(a, exprType ((typedVar var).InitExpr), id)

            | _ ->    
                failwith "delegates not yet implemented"

        | Syntax.Float(a, n) -> Float(a, n)
        | Syntax.Int(a, n) -> Int(a, n)

        | Syntax.List(a, values) ->
            match tryParseAsm expr, values with
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
                    ApplyFunc(a, blockType (typedBlock !func.Block), id, List.map (typedExpr env) args)

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
