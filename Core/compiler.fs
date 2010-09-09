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

    let compileToAst (code : Syntax.Expr<_> list) : EnvValue<Expr<_>> =
        let values =
            ["Console.WriteLine", NetFunc <| typeof<Console>.GetMethod("WriteLine", Array.empty)
             "=",                 EqFunc
             "if",                IfFunc]
            |> Map.ofList

        let env = { Env.empty with Values = values }

        code
        |> makeFunc env "main" List.empty
        |> Func
        |> typedValue

    let compileAstToDelegate (delegateType : Type) (main : EnvValue<Expr<_>>) : Delegate =
        let name = AssemblyName("DynamicAssembly")
        let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(name.Name + ".dll")
        let typeBuilder = moduleBuilder.DefineType("Program")
        let ilFuncs = foldValue (makeILFunction typeBuilder) Map.empty "main" main

        for _, ilFunc in Map.toSeq ilFuncs do
            emitFunc ilFuncs ilFunc

        let t = typeBuilder.CreateType()
        assemblyBuilder.Save(name.Name + ".dll")
        let methodInfo = t.GetMethod("main", BindingFlags.Public ||| BindingFlags.Static)
        Delegate.CreateDelegate(delegateType, methodInfo)

    let compileToDelegate (delegateType : Type) (code : Syntax.Expr<_> list) : Delegate =
        code
        |> compileToAst
        |> compileAstToDelegate delegateType
