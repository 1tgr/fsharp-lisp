#light
namespace Tim.Lisp.Core

module Syntax =
    type Expr<'a> = Atom of 'a * string
                  | List of 'a * Expr<'a> list
                  | Number of 'a * int
                  | String of 'a * string
