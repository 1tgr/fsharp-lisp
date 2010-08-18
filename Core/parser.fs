#light
namespace Tim.Lisp.Core
open System
open FParsec
open FParsec.CharParsers
open FParsec.Error
open FParsec.Primitives
open Tim.Lisp.Core.Syntax

module internal ParserImpl =
    let ch c = skipChar c >>. spaces

    let node (f : (Position * 'a) -> 'c) (p : Parser<'a, 'b>) : Parser<'c, 'b> =
        pipe2 getPosition p (fun a b -> f(a, b))

    let atom : Parser<Expr<Position>, unit> =
        many1Chars (choice [asciiLetter; digit; anyOf "=+-*/."]) .>> spaces |> node Atom

    let number : Parser<Expr<Position>, unit> =
        let numberNode (position : Position) (literal : NumberLiteral) : Expr<Position> =
            if literal.IsInteger then
                Int(position, int literal.String)
            else
                Float(position, float literal.String)

        let numberFormat = NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowExponent
        pipe2 getPosition (numberLiteral numberFormat "number" .>> spaces) numberNode

    let (expr : Parser<Expr<Position>, unit>, exprRef) =
        createParserForwardedToRef ()

    let list : Parser<Expr<Position>, unit> =
        ch '(' >>. many expr .>> ch ')' |> node List

    let string : Parser<Expr<Position>, unit> =
        ch '"' >>. manyCharsTill anyChar (ch '"') |> node String

    exprRef := choice [number; list; string; atom]

    let prog : Parser<Expr<Position> list, unit> =
        spaces >>. many expr .>> eof

module Parser =
    let parseString (s : string) : Expr<Position> list =
        match runParserOnString ParserImpl.prog () "<<input>>" s with
        | Success(e, _, _) -> e
        | Failure(msg, _, _) -> failwith msg
