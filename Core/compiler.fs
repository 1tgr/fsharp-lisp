#light
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Reflection.Emit

module Compiler =
    open CodeGen
    open Scoped
    open Typed

    let mainEnvId = nextId ()
    
    let mainEnv : Env<_, _> =
        let values =
            ["Console.WriteLine", NetFunc <| typeof<Console>.GetMethod("WriteLine", Array.empty)
             "=",                 EqFunc
             "if",                IfFunc]
            |> Map.ofList

        { Env.empty with Values = values
                         Using = Set.singleton "" }

    let prelude : Syntax.Expr list =
        Parser.parseFile (Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "stdlib.scm"))

    let compileToAst (code : Syntax.Expr list) : Map<EnvId, Env<Expr, _>> * EnvValue<Expr, _> =
        let envs = ref (Map.add mainEnvId mainEnv Map.empty)
        let ast = makeFunc envs mainEnvId "main" List.empty (prelude @ code)
        let typedEnvs = Map.map (fun _ -> typedEnv !envs) !envs
        let typedAst = typedValue !envs ast
        typedEnvs, typedAst

    let compileAstToMemory (filename : string) (envs : Map<EnvId, Env<_, _>>, main : EnvValue<Expr, _>) : (AssemblyBuilder * Type * MethodInfo) =
        let name = AssemblyName(Path.GetFileNameWithoutExtension(filename))

        let assemblyBuilder =
            match Path.GetDirectoryName(filename) with
            | "" -> AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndSave)
            | dir -> AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndSave, dir)

        let moduleBuilder = assemblyBuilder.DefineDynamicModule(Path.GetFileName(filename))
        let typeBuilder = moduleBuilder.DefineType("Program", TypeAttributes.Sealed ||| TypeAttributes.Public)

        let ilFuncs = makeILFunction envs typeBuilder Map.empty "main" main
        let ilFuncs = Map.fold (fun map _ env -> Map.fold (makeILFunction envs typeBuilder) map env.Values) ilFuncs envs

        for _, ilFunc in Map.toSeq ilFuncs do
            emitFunc envs ilFuncs ilFunc

        let t = typeBuilder.CreateType()
        let methodInfo = t.GetMethod("main", BindingFlags.Public ||| BindingFlags.Static)
        assemblyBuilder.SetEntryPoint(methodInfo)
        assemblyBuilder, t, methodInfo

    let compileAstToDelegate (delegateType : Type) (ast : Map<EnvId, Env<_, _>> * EnvValue<Expr, _>) : Delegate =
        let _, _, methodInfo = compileAstToMemory "DynamicAssembly" ast
        Delegate.CreateDelegate(delegateType, methodInfo)

    let compileAstToFile (filename : string) (ast : Map<EnvId, Env<_, _>> * EnvValue<Expr, _>) : unit =
        let assemblyBuilder, _, _ = compileAstToMemory filename ast
        assemblyBuilder.Save(filename)

    let compileToMemory (filename : string) (code : Syntax.Expr list) : (AssemblyBuilder * Type * MethodInfo) =
        code
        |> compileToAst
        |> compileAstToMemory filename

    let compileToDelegate (delegateType : Type) (code : Syntax.Expr list) : Delegate =
        code
        |> compileToAst
        |> compileAstToDelegate delegateType

    let compileToFile (filename : string) (code : Syntax.Expr list) : unit =
        code
        |> compileToAst
        |> compileAstToFile filename
