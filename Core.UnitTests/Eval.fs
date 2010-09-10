#light
namespace Tim.Lisp.Core.UnitTests
open System
open Tim.Lisp.Core

module Eval =
    let builtins =
        @"
(.using System)"
        |> Parser.parseString

    let ast s =
        Compiler.compileToAst (builtins @ Parser.parseString s)

    let eval<'T> s =
        let a = ast s
        let d = Compiler.compileAstToDelegate typeof<Func<'T>> a
        let f = d :?> Func<'T>
        f.Invoke()

    let evalVoid s =
        let a = ast s
        let d = Compiler.compileAstToDelegate typeof<Action> a
        let a = d :?> Action
        a.Invoke()
