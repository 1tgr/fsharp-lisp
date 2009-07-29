#light
namespace Tim.Lisp.Core.UnitTests
open System
open Tim.Lisp.Core

module Eval =
    let eval<'T> s =
        let d =
            s
            |> Parser.parseString
            |> Compiler.compileToDelegate typeof<Func<'T>>
        let f = d :?> Func<'T>
        f.Invoke()

    let evalVoid s =
        let d =
            s
            |> Parser.parseString
            |> Compiler.compileToDelegate typeof<Action>
        let a = d :?> Action
        a.Invoke()
