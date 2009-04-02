#light
namespace Tim.Lisp.Compiler

open System
open System.Reflection
open System.Reflection.Emit
open Tim.Lisp.Core

module Program =
    let main =
        try
            let code = 
                [
                    List [ Atom "-"; List [ Atom "+"; Number 4; Number 5; Number 6 ]; Number 2; Number 3 ];
                    List [ Atom "+"; List [ Atom "eval"; List [ Atom "quote"; String "42"; ] ]; Number 1 ]
                ]

            let assemblyName = AssemblyName "output"
            let a = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Save)
            let m = a.DefineDynamicModule (assemblyName.Name + ".exe")
            let t = m.DefineType("Program", TypeAttributes.Sealed ||| TypeAttributes.Public)
            let meth = t.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<Void>, [| |])
            let generator = meth.GetILGenerator()

            let compile emptyEnv x =
                let withPrimitives = x |> Evaluator.insertPrimitives
                let env = withPrimitives |> Compiler.compile generator emptyEnv
                match Compiler.typeOf env withPrimitives with
                | t when t.IsValueType -> generator.Emit(OpCodes.Box, t)
                | _ -> ()
                generator.Emit(OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<obj> |]))
                env

            List.fold_left compile Map.empty code |> ignore
            generator.Emit OpCodes.Ret
            t.CreateType() |> ignore
            a.SetEntryPoint(meth)
            a.Save (assemblyName.Name + ".exe")
            0
        with ex ->
            stderr.WriteLine(ex)
            1