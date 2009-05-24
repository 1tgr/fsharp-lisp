#light
namespace Tim.Lisp.Core
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing

module Parser =
    let parseString (s : string) = FSYacc.parse FSLex.tokenize <| LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(s))
