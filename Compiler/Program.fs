#light
namespace Tim.Lisp.Compiler

open System
open System.Reflection
open System.Reflection.Emit
open Tim.Lisp.Core

module Program =
    [<EntryPoint>]
    let main (_ : string array) =
        try
            let code = 
                [
                    List [
                        Atom "define"; 
                        Atom "fact"; 
                        List [ 
                            Atom "lambda"; 
                            List [ Atom "n" ]; 
                            List [ 
                                Atom "if"; 
                                List [
                                    Atom "=";
                                    Atom "n";
                                    Number 0
                                ];
                                Number 1;
                                List [
                                    Atom "*";
                                    Atom "n";
                                    List [
                                        Atom "fact";
                                        List [
                                            Atom "-";
                                            Atom "n";
                                            Number 1
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ];
                    List [
                        Atom "fact";
                        Number 6
                    ]
                ]

            let assemblyName = AssemblyName "output"
            let a = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Save)
            let m = a.DefineDynamicModule (assemblyName.Name + ".exe")
            let typeBuilder = m.DefineType("Program", TypeAttributes.Sealed ||| TypeAttributes.Public)
            let meth = typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<Void>, [| |])
            let generator = meth.GetILGenerator()

            let compile emptyEnv x =
                let withPrimitives = x |> Evaluator.insertPrimitives
                let env = withPrimitives |> Compiler.compile generator typeBuilder emptyEnv
                match Compiler.typeOf env withPrimitives with
                | t when t = typeof<Void> -> ()
                | t ->
                    if t.IsValueType then generator.Emit(OpCodes.Box, t) else ()
                    generator.Emit(OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<obj> |]))
                env

            List.fold_left compile Map.empty code |> ignore
            generator.Emit OpCodes.Ret
            typeBuilder.CreateType() |> ignore
            a.SetEntryPoint(meth)
            a.Save (assemblyName.Name + ".exe")
            0
        with ex ->
            stderr.WriteLine(ex)
            1