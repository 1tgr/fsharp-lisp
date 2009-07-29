#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open ILBlockModule

module Compiler =
    let compile expectedReturnType target code =
        let rec tryLast = function
            | [ item ] -> Some item
            | item :: items -> tryLast items
            | [ ] -> None

        let env, head, tail = code |> CodeGenerator.compile target Map.empty
        let epilogBlock = new ILBlock()

        let returnType = 
            match code |> tryLast with
            | Some value -> CodeGenerator.typeOf env value
            | None -> typeof<Void>

        if expectedReturnType = typeof<Void> && returnType <> typeof<Void> then
            emit epilogBlock Pop
        else if (not expectedReturnType.IsValueType) && returnType.IsValueType then
            emit epilogBlock (Box returnType)

        tail.Branch <- Br epilogBlock
        epilogBlock.Branch <- Ret
        target.GenerateIL head

    let compileToDelegate (delegateType : #Type) code =
        let returnType = delegateType.GetMethod("Invoke", BindingFlags.Public ||| BindingFlags.Instance).ReturnType
        let meth = new DynamicMethod("Main", returnType, Type.EmptyTypes)
        let target = new DynamicMethodTarget(meth)
        compile returnType target code
        meth.CreateDelegate(delegateType)

    let compileToMemory assemblyName code =
        let a = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
        let m = a.DefineDynamicModule (assemblyName.Name + ".exe")
        let typeBuilder = m.DefineType("Program", TypeAttributes.Sealed ||| TypeAttributes.Public)
        let methodBuilder = typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<Void>, [| |])
        let target = new MethodBuilderTarget(typeBuilder, methodBuilder)
        compile typeof<Void> target code

        let concreteType = typeBuilder.CreateType()
        a.SetEntryPoint(methodBuilder)
        (a, concreteType, methodBuilder)

    let compileToFile filename code =
        let assemblyName = new AssemblyName (Path.GetFileNameWithoutExtension filename)
        let (a, _, _) = compileToMemory assemblyName code
        a.Save filename
