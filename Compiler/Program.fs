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
            @"
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(Console.WriteLine ""6! = {0}"" (fact 6))
(Console.WriteLine ""What is your name?"" """")
(Console.WriteLine ""Hello, {0}"" (Console.ReadLine))"
            |> Parser.parseString 
            |> Compiler.compileToFile "output.exe" 
            0
        with ex ->
            stderr.WriteLine(ex)
            1