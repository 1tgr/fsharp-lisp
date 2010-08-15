#light
namespace Tim.Lisp.Core
open System
open Tim.Lisp.Core.Syntax

module Compiler =
    let compileToDelegate (delegateType : Type) (code : Expr<_> list) : Delegate =
        raise <| NotImplementedException(sprintf "%A" code)
