#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Reflection.Emit

module Compiler =
    let importClrType (clrType : Type) env =
        let isParamArray (parameterInfo : #ParameterInfo) = parameterInfo.IsDefined(typeof<ParamArrayAttribute>, true)
        let makeLambdaRef (methodInfo : #MethodInfo) =
            let parameters = methodInfo.GetParameters()
            let parameterTypes = parameters |> List.of_array |> List.map (function
                | p when p.ParameterType.IsArray && isParamArray p -> p.ParameterType.GetElementType()
                | p -> p.ParameterType)
            let isParamArray = parameters.Length > 0 && isParamArray parameters.[parameters.Length - 1]
            LambdaRef (methodInfo, isParamArray, parameterTypes)
        clrType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
        |> (env
            |> Array.fold_left (fun env' m -> Map.add (clrType.Name + "." + m.Name) (makeLambdaRef m) env'))

    let compileToMemory assemblyName code =
        let a = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
        let m = a.DefineDynamicModule (assemblyName.Name + ".exe")
        let typeBuilder = m.DefineType("Program", TypeAttributes.Sealed ||| TypeAttributes.Public)
        let meth = typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<Void>, [| |])
        let generator = meth.GetILGenerator()

        let compile env v = 
            Console.WriteLine(any_to_string v)
            v |> Evaluator.insertPrimitives |> CodeGenerator.compile generator typeBuilder env

        Console.WriteLine(any_to_string code)
        code 
        |> (Map.empty 
            |> importClrType (typeof<Console>) |> List.fold_left compile) 
        |> ignore

        generator.Emit OpCodes.Ret
        let concreteType = typeBuilder.CreateType()
        a.SetEntryPoint(meth)
        (a, concreteType, meth)

    let compileToFile filename code =
        let assemblyName = new AssemblyName (Path.GetFileNameWithoutExtension filename)
        let (a, _, _) = compileToMemory assemblyName code
        a.Save filename
