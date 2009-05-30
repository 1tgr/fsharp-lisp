#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Reflection.Emit

module Compiler =
    let compile expectedReturnType generator defineMethod code =
        let rec tryLast = function
            | [ item ] -> Some item
            | item :: items -> tryLast items
            | [ ] -> None

        let compile' env = CodeGenerator.insertPrimitives >> CodeGenerator.compile generator defineMethod env
        let env = code |> List.fold compile' Map.empty

        let returnType = 
            match code |> tryLast with
            | Some value -> CodeGenerator.typeOf env value
            | None -> typeof<Void>

        if expectedReturnType = typeof<Void> && returnType <> typeof<Void> then
            generator.Emit OpCodes.Pop
        else if (not expectedReturnType.IsValueType) && returnType.IsValueType then
            generator.Emit(OpCodes.Box, returnType)

        generator.Emit OpCodes.Ret

    let compileToDelegate (delegateType : #Type) code =
        let returnType = delegateType.GetMethod("Invoke", BindingFlags.Public ||| BindingFlags.Instance).ReturnType
        let meth = new DynamicMethod("Main", returnType, Type.EmptyTypes)
        let generator = meth.GetILGenerator()

        let defineMethod name returnType parameterTypes = 
            let methodInfo = new DynamicMethod(name, returnType, Array.of_list parameterTypes)
            (methodInfo, methodInfo.GetILGenerator())

        compile returnType generator defineMethod code
        meth.CreateDelegate(delegateType)

    let compileToMemory assemblyName code =
        let a = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
        let m = a.DefineDynamicModule (assemblyName.Name + ".exe")
        let typeBuilder = m.DefineType("Program", TypeAttributes.Sealed ||| TypeAttributes.Public)
        let meth = typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<Void>, [| |])
        let generator = meth.GetILGenerator()

        let defineMethod name returnType parameterTypes = 
            let methodInfo = typeBuilder.DefineMethod(name, MethodAttributes.Static ||| MethodAttributes.Private, returnType, Array.of_list parameterTypes)
            (methodInfo, methodInfo.GetILGenerator())

        compile typeof<Void> generator defineMethod code

        let concreteType = typeBuilder.CreateType()
        a.SetEntryPoint(meth)
        (a, concreteType, meth)

    let compileToFile filename code =
        let assemblyName = new AssemblyName (Path.GetFileNameWithoutExtension filename)
        let (a, _, _) = compileToMemory assemblyName code
        a.Save filename
