#light
namespace Tim.Lisp.Core

module Syntax =
    type Expr<'a> = Atom of 'a * string
                  | Float of 'a * float
                  | Int of 'a * int
                  | List of 'a * Expr<'a> list
                  | String of 'a * string
