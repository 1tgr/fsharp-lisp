#light
namespace Tim.Lisp.Core
open FParsec

module Syntax =
    type Expr = Atom of Position * string
              | Float of Position * float
              | Int of Position * int
              | List of Position * Expr list
              | String of Position * string
