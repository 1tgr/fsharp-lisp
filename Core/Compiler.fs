#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Reflection.Emit

module Compiler =
    let compileToMemory assemblyName code =
        let a = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
        let m = a.DefineDynamicModule (assemblyName.Name + ".exe")
        let typeBuilder = m.DefineType("Program", TypeAttributes.Sealed ||| TypeAttributes.Public)
        let meth = typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<Void>, [| |])
        let generator = meth.GetILGenerator()

        let compile env v = 
            printfn "%A" v
            v |> Evaluator.insertPrimitives |> CodeGenerator.compile generator typeBuilder env

        printfn "%A" code
        code 
        |> (Map.empty |> List.fold compile) 
        |> ignore

        generator.Emit OpCodes.Ret
        let concreteType = typeBuilder.CreateType()
        a.SetEntryPoint(meth)
        (a, concreteType, meth)

    let compileToFile filename code =
        let assemblyName = new AssemblyName (Path.GetFileNameWithoutExtension filename)
        let (a, _, _) = compileToMemory assemblyName code
        a.Save filename
