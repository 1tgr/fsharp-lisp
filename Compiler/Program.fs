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
(.using System)
(define (countTo total acc)
  (if (= total acc)
    acc
    (countTo total (+ 1 acc))))
(define (factorial n acc)
  (if (= n 0)
    acc
    (factorial (- n 1) (* acc n))))
(define number 6)
(define (display-int n)
        (.asm (call Console.WriteLine Int32) Void n))
(display-int (factorial (countTo number 0) 1))"

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