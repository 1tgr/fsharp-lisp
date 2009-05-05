#light
namespace Tim.Lisp.Core
open System.IO
open System.Text

module Parser =
    let parseTextReader reader =
        let lexbuf = Lexing.from_text_reader Encoding.UTF8 reader
        FSYacc.parse FSLex.tokenize lexbuf

    let parseString s =
        use reader = new StringReader(s)
        parseTextReader reader
