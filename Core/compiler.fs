#light
namespace Tim.Lisp.Core
open System
open Tim.Lisp.Core.Syntax

module Compiler =
    let compileToDelegate (delegateType : Type) (expr : Expr<_>) : Delegate =
        raise <| NotImplementedException()
