#light
namespace Tim.Lisp.Core
open System
open System.Text
open FParsec
open FParsec.CharParsers
open FParsec.Error
open FParsec.Primitives
open Tim.Lisp.Core.Syntax

module internal ParserImpl =
    let ch c = skipChar c >>. spaces

    let node (f : (Position * 'a) -> 'c) (p : Parser<'a, 'b>) : Parser<'c, 'b> =
        pipe2 getPosition p (fun a b -> f(a, b))

    let atom : Parser<Expr, unit> =
        many1Chars (choice [asciiLetter; digit; anyOf @".!#\$%&|*+-/:<=>?@^_~"]) .>> spaces |> node Atom

    let number : Parser<Expr, unit> =
        let numberNode (position : Position) (literal : NumberLiteral) : Expr =
            if literal.IsInteger then
                Int(position, int literal.String)
            else
                Float(position, float literal.String)

        let numberFormat = NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowExponent
        pipe2 getPosition (numberLiteral numberFormat "number" .>> spaces) numberNode

    let (expr : Parser<Expr, unit>, exprRef) =
        createParserForwardedToRef ()

    let list : Parser<Expr, unit> =
        ch '(' >>. many expr .>> ch ')' |> node List

    let string : Parser<Expr, unit> =
        ch '"' >>. manyCharsTill anyChar (ch '"') |> node String

    exprRef := choice [number; list; string; atom]

    let prog : Parser<Expr list, unit> =
        spaces >>. many expr .>> eof

module Parser =
    let parseString (s : string) : Expr list =
        match runParserOnString ParserImpl.prog () "<<input>>" s with
        | Success(e, _, _) -> e
        | Failure(msg, _, _) -> failwith msg

    let parseFile (filename : string) : Expr list =
        match runParserOnFile ParserImpl.prog () filename Encoding.UTF8 with
        | Success(e, _, _) -> e
        | Failure(msg, _, _) -> failwith msg