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

    let mainEnv : Env<Syntax.Expr> =
        let values =
            ["Console.WriteLine", NetFunc <| typeof<Console>.GetMethod("WriteLine", Array.empty)
             "=",                 EqFunc
             "if",                IfFunc]
            |> Map.ofList

        { Env.empty with Values = values
                         Using = Set.singleton "" }

    let prelude : Syntax.Expr list =
        Parser.parseFile (Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "stdlib.scm"))

    let compileToAst (code : Syntax.Expr list) : EnvValue<Expr> =
        prelude @ code
        |> makeFunc mainEnv "main" List.empty
        |> Func
        |> typedValue

    let compileAstToMemory (filename : string) (main : EnvValue<Expr>) : (AssemblyBuilder * Type * MethodInfo) =
        let name = AssemblyName(Path.GetFileNameWithoutExtension(filename))
        let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndSave, Path.GetDirectoryName(filename))
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(Path.GetFileName(filename))
        let typeBuilder = moduleBuilder.DefineType("Program", TypeAttributes.Sealed ||| TypeAttributes.Public)
        let ilFuncs = foldValue (makeILFunction typeBuilder) Map.empty "main" main

        for _, ilFunc in Map.toSeq ilFuncs do
            emitFunc ilFuncs ilFunc

        let t = typeBuilder.CreateType()
        let methodInfo = t.GetMethod("main", BindingFlags.Public ||| BindingFlags.Static)
        assemblyBuilder.SetEntryPoint(methodInfo)
        assemblyBuilder, t, methodInfo

    let compileAstToDelegate (delegateType : Type) (main : EnvValue<Expr>) : Delegate =
        let _, _, methodInfo = compileAstToMemory "DynamicAssembly" main
        Delegate.CreateDelegate(delegateType, methodInfo)

    let compileAstToFile (filename : string) (main : EnvValue<Expr>) : unit =
        let assemblyBuilder, _, _ = compileAstToMemory filename main
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
