#light
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open Tim.Lisp.Core.Syntax

module internal CompilerImpl =
    type Block<'a> =
        {
            Body : Stmt<'a> list
        }

    and Stmt<'a> = Block of Env<'a> * Block<'a>
                 | Expr of Env<'a> * Expr<'a>

    and Func<'a> =
        {
            Block : Block<'a>
            Params : string list
        }

    and EnvValue<'a> = Func of Func<'a>
                     | Var of Expr<'a>

    and Env<'a> =
        {
            Parent : Env<'a> option
            Func : Func<'a> ref
            Values : Map<string, EnvValue<'a>>
        }

    let makeBlock (env : Env<'a>) (exprs : Expr<'a> list) : Env<'a> * Block<'a> =
        let rec addToScope
            (env   : Env<'a>, block : Block<'a>) 
            (exprs : Expr<'a> list) 
                   : Env<'a> * Block<'a>
            =
            match exprs with
            | List(_, Atom(_, "define") :: values) :: tail ->
                let name, value = 
                    match values with
                    | [Atom(_, name); value] -> name, Var value
                    | List _ :: _ -> failwithf "Functions not yet implemented"
                    | _ -> failwithf "define expected 1 value, not %A" values

                match block with
                | { Body = [] } ->
                    tail |> addToScope ({ env with Values = Map.add name value env.Values }, { Body = [] })

                | _ ->
                    let ienv = { env with Parent = Some env
                                          Values = Map.ofList [(name, value)] }

                    let ienv, iblock = tail |> addToScope (ienv, { Body = [] })
                    env, { block with Body = block.Body @ [Block(ienv, iblock)] }

            | head :: tail ->
                let scope = env, { block with Body = block.Body @ [Expr(env, head)] }
                tail |> addToScope scope

            | [] ->
                env, block

        addToScope (env, { Body = [] }) exprs

    type ILFunction(typeBuilder : TypeBuilder, name : string, paramNames : string list) =
        let dynamicMethod = 
            typeBuilder.DefineMethod(
                name, 
                MethodAttributes.Public ||| MethodAttributes.Static, 
                typeof<Void>, 
                paramNames |> List.map (fun _ -> typeof<int>) |> Array.ofList)

        let generator = dynamicMethod.GetILGenerator()

        member this.DynamicMethod = dynamicMethod
        member this.Generator = generator

    let rec makeILFunctionsFromValue (typeBuilder : TypeBuilder) (name : string) (value : EnvValue<'a>) : (ILFunction * Func<'a>) list =
        match value with
        | Func func ->
            let ilFunc = ILFunction(typeBuilder, name, func.Params)
            (ilFunc, func) :: List.collect (makeILFunctions typeBuilder) func.Block.Body

        | Var _ ->
            []

    and makeILFunctions (typeBuilder : TypeBuilder) (stmt : Stmt<'a>) : (ILFunction * Func<'a>) list =
        match stmt with
        | Block(env, block) ->
            env.Values
            |> Map.toList
            |> List.collect (fun (a, b) -> makeILFunctionsFromValue typeBuilder a b)
            |> List.append (List.collect (makeILFunctions typeBuilder) block.Body)

        | Expr _ ->
            []

    let emitFunc (ilFunc : ILFunction) (func : Func<_>) : unit =
        let emitExpr (expr : Expr<_>) : unit =
            ()

        let rec emitStmt (stmt : Stmt<_>) : unit =
            match stmt with
            | Block(_, block) ->
                List.iter emitStmt block.Body

            | Expr(_, expr) ->
                emitExpr expr

        List.iter emitStmt func.Block.Body
        ilFunc.Generator.Emit(OpCodes.Ret)

open CompilerImpl

module Compiler =
    let compileToDelegate (delegateType : Type) (code : Expr<'a> list) : Delegate =
        let main =
            let mainRef = ref { Block = { Body = [] }; Params = [] }
            let mainEnv, mainBlock = makeBlock { Parent = None; Func = mainRef; Values = Map.empty } code
            mainRef := { !mainRef with Block = mainBlock }
            Func !mainRef

        let name = AssemblyName("DynamicAssembly")
        let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(name.Name + ".dll")
        let typeBuilder = moduleBuilder.DefineType("Program")
        
        for (ilFunc, func) in makeILFunctionsFromValue typeBuilder "main" main do
            emitFunc ilFunc func

        let t = typeBuilder.CreateType()
        assemblyBuilder.Save(name.Name + ".dll")
        let methodInfo = t.GetMethod("main", BindingFlags.Public ||| BindingFlags.Static)
        Delegate.CreateDelegate(delegateType, methodInfo)
