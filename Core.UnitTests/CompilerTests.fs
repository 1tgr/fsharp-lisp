#light
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
    member this.asm_NoArg_NoStack() =
        eval "(.asm ldc.i4.0 System.Int32)" |> shouldEqual 0

    [<Test>]
    member this.asm_Arg_NoStack() =
        eval "(.asm (ldc.i4 6) System.Int32)" |> shouldEqual 6

    [<Test>]
    member this.asm_NoArg_Stack() =
        eval "(.asm add System.Int32 2 4)" |> shouldEqual 6

    [<Test>]
    member this.asm_Arg_Stack() =
        eval @"(.asm (call System.Char.ToUpperInvariant) System.Char 88)" |> shouldEqual 'X'

    [<Test>]
    member this.shouldNestAsm() =
        eval @"
(.asm add System.Int32
    (.asm (ldc.i4 2) System.Int32) 
    (.asm (ldc.i4 4) System.Int32))" 
        |> shouldEqual 6

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
