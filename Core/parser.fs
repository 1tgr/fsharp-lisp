#light
namespace Tim.Lisp.Core
open FParsec
open Tim.Lisp.Core.Syntax

module Parser =
    let parseString (s : string) : Expr<Position> =
        raise <| System.NotImplementedException()
