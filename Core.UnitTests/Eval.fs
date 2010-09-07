#light
namespace Tim.Lisp.Core.UnitTests
open System
open Tim.Lisp.Core

module Eval =
    let builtins =
        @"
(.ref ""nunit.framework.dll"")
(.using System)
(.using NUnit.Framework)
(define (+ a b) (.asm add Int32 a b))
(define (- a b) (.asm sub Int32 a b))
(define (* a b) (.asm mul Int32 a b))
(define (assert-equal a b) (.asm (call Assert.AreEqual Int32 Int32) Void a b))"
        |> Parser.parseString

    let eval<'T> s =
        let d =
            builtins @ Parser.parseString s
            |> Compiler.compileToDelegate typeof<Func<'T>>
        let f = d :?> Func<'T>
        f.Invoke()

    let evalVoid s =
        let d =
            builtins @ Parser.parseString s
            |> Compiler.compileToDelegate typeof<Action>
        let a = d :?> Action
        a.Invoke()
