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
                    (*List [
                        Atom "define"; 
                        List [
                            Atom "fact"; 
                            Atom "n"
                        ];
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
                    ];
                    List [
                        Atom "WriteLine";
                        List [
                            Atom "fact";
                            Number 6
                        ]
                    ];*)
                    List [
                        Atom "WriteLine";
                        String "What is your name?";
                        String ""
                    ];
                    List [
                        Atom "WriteLine";
                        String "Hello, {0}";
                        List [ Atom "ReadLine" ]
                    ]
                ]

            let assemblyName = AssemblyName "output"
            let a = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Save)
            let m = a.DefineDynamicModule (assemblyName.Name + ".exe")
            let typeBuilder = m.DefineType("Program", TypeAttributes.Sealed ||| TypeAttributes.Public)
            let meth = typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<Void>, [| |])
            let generator = meth.GetILGenerator()

            let compile env = Evaluator.insertPrimitives >> Compiler.compile generator typeBuilder env
            let initialEnv = typeof<Console>.GetMethods() |> Array.fold_left (fun env m -> Map.add m.Name (LambdaRef m) env) Map.empty

            List.fold_left compile initialEnv code |> ignore
            generator.Emit OpCodes.Ret
            typeBuilder.CreateType() |> ignore
            a.SetEntryPoint(meth)
            a.Save (assemblyName.Name + ".exe")
            0
        with ex ->
            stderr.WriteLine(ex)
            1