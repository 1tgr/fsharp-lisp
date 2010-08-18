#light
namespace Tim.Lisp.Core.UnitTests
open System
open Tim.Lisp.Core

module Eval =
    let builtins =
        @"
(define (+ a b) (.asm add System.Int32 a b))
(define (- a b) (.asm sub System.Int32 a b))
(define (* a b) (.asm mul System.Int32 a b))"
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
