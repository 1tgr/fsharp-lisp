#light
namespace Tim.Lisp.Core.UnitTests
open System
open Xunit
open Assertion
open Eval

module CompilerTests =
    [<Fact>]
    let shouldReturnNumber() = 
        eval "6" |> shouldEqual 6
    
    [<Fact>]
    let shouldReturnString() = 
        eval "\"hello\"" |> shouldEqual "hello"

    [<Fact>]
    let shouldDefineAndRetrieveValue() =
        eval "(define number 6) number" |> shouldEqual 6

    [<Fact>]
    let asm_NoArg_NoStack() =
        eval "(.asm ldc.i4.0 Int32)" |> shouldEqual 0

    [<Fact>]
    let asm_Arg_NoStack() =
        eval "(.asm (ldc.i4 6) Int32)" |> shouldEqual 6

    [<Fact>]
    let asm_NoArg_Stack() =
        eval "(.asm add Int32 2 4)" |> shouldEqual 6

    [<Fact>]
    let asm_Arg_Stack() =
        eval @"
(define (char-upcase c) (.asm (call Char.ToUpperInvariant Char) Char c))
(char-upcase #\x)" |> shouldEqual 'X'

    [<Fact>]
    let shouldNestAsm() =
        eval @"
(.asm add Int32
    (.asm (ldc.i4 2) Int32) 
    (.asm (ldc.i4 4) Int32))" 
        |> shouldEqual 6

    [<Fact>]
    let givenNonTailRecursiveFunction_ShouldNotTailCall() =
        eval @"
(define (factorial n)
  (if (= n 0) 
    1 
    (* n (factorial (- n 1)))))
(factorial 6)"
        |> shouldEqual 720

    [<Fact>]
    let givenTailRecursiveFunction_ShouldTailCall() =
        eval @"
(define (factorial n acc)
  (if (= n 0)
    acc
    (factorial (- n 1) (* acc n))))
(factorial 6 1)"
        |> shouldEqual 720

    [<Fact>]
    let givenTailRecursiveFunction_ShouldNotOverflowStack() =
        eval @"
(define (countTo total acc)
  (if (= total acc)
    acc
    (countTo total (+ 1 acc))))
(countTo 10000000 0)" |> shouldEqual 10000000

    [<Fact>]
    let shouldSelectMethodZeroArgs() =
        evalVoid "(Console.WriteLine)"

    [<Fact>]
    let shouldSelectMethodOneArgReferenceType() =
        evalVoid "(Console.WriteLine \"hello\")"

    [<Fact>]
    let shouldSelectMethodOneArgBoxed() =
        evalVoid "(Console.WriteLine 6)"

    [<Fact>]
    let shouldSelectMethodParamsArrayReferenceType() =
        evalVoid "(Console.WriteLine \"hello {0}\" \"world\")"

    [<Fact>]
    let shouldSelectMethodParamsArrayBoxed() =
        evalVoid "(Console.WriteLine \"hello {0}\" 6)"
