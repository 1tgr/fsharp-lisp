﻿#light
namespace Tim.Lisp.Core.UnitTests
open System
open NUnit.Framework
open Assertion
open Eval

[<TestFixture>]
type CompilerTests() =
    [<Test>]
    member this.shouldReturnNumber() = 
        eval "6" |> shouldEqual 6
    
    [<Test>]
    member this.shouldReturnString() = 
        eval "\"hello\"" |> shouldEqual "hello"

    [<Test>]
    member this.shouldDefineAndRetrieveValue() =
        eval "(define number 6) number" |> shouldEqual 6

    [<Test>]
    member this.givenNonTailRecursiveFunction_ShouldNotTailCall() =
        eval @"
(define (factorial n)
  (if (= n 0) 
    1 
    (* n (factorial (- n 1)))))
(factorial 6)"
        |> shouldEqual 720

    [<Test>]
    member this.givenTailRecursiveFunction_ShouldTailCall() =
        eval @"
(define (factorial n acc)
  (if (= n 0)
    acc
    (factorial (- n 1) (* acc n))))
(factorial 6 1)"
        |> shouldEqual 720

    [<Test>]
    member this.givenTailRecursiveFunction_ShouldNotOverflowStack() =
        eval @"
(define (countTo total acc)
  (if (= total acc)
    acc
    (countTo total (+ 1 acc))))
(countTo 10000000 0)" |> shouldEqual 10000000

    [<Test>]
    member this.shouldSelectMethodZeroArgs() =
        evalVoid "(Console.WriteLine)"

    [<Test>]
    member this.shouldSelectMethodOneArgReferenceType() =
        evalVoid "(Console.WriteLine \"hello\")"

    [<Test>]
    member this.shouldSelectMethodOneArgBoxed() =
        evalVoid "(Console.WriteLine 6)"

    [<Test>]
    member this.shouldSelectMethodParamsArrayReferenceType() =
        evalVoid "(Console.WriteLine \"hello {0}\" \"world\")"

    [<Test>]
    member this.shouldSelectMethodParamsArrayBoxed() =
        evalVoid "(Console.WriteLine \"hello {0}\" 6)"
