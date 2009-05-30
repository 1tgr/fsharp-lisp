#light
namespace Tim.Lisp.Compiler

open System
open System.CodeDom.Compiler
open Tim.Lisp.Core

module Program =
    [<EntryPoint>]
    let main (_ : string array) =
        try
            let code = @"
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(Console.WriteLine ""6! = {0}"" (fact 6))
(Console.WriteLine ""What is your name?"")
(Console.WriteLine ""Hello, {0}"" (Console.ReadLine))"

            let provider = new LispCodeProvider()
            let options = new CompilerParameters()
            options.OutputAssembly <- "output.exe"

            let results = provider.CompileAssemblyFromSource(options, [| code |])
            let errors = results.Errors |> Seq.cast<CompilerError>
            errors |> Seq.iter (fun e -> stderr.WriteLine(e.ErrorText))

            if errors |> Seq.exists (fun e -> not e.IsWarning)
            then 1
            else 0
        with ex ->
            stderr.WriteLine(ex)
            1