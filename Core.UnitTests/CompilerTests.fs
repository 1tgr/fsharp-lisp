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
    member this.shouldDefineAndInvokeFunction() =
        eval "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 6)" |> shouldEqual 720

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
