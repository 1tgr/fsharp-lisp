#light
namespace Tim.Lisp.Core

module Syntax =
    type Expr<'a> = Atom of 'a * string
                  | Float of 'a * float
                  | Int of 'a * int
                  | List of 'a * Expr<'a> list
                  | String of 'a * string

    module Expr =
        let rec annot (a : Expr<'a>) : 'a =
            match a with
            | Atom (a, _) -> a
            | Float (a, _) -> a
            | Int (a, _) -> a
            | List (a, _) -> a
            | String (a, _) -> a

        let rec mapa (fn : 'a -> 'b) (a : Expr<'a>) : Expr<'b> =
            match a with
            | Atom (a, x) -> Atom (fn a, x)
            | Float (a, x) -> Float (fn a, x)
            | Int (a, x) -> Int (fn a, x)
            | List (a, x) -> List (fn a, List.map (mapa fn) x)
            | String (a, x) -> String (fn a, x)
