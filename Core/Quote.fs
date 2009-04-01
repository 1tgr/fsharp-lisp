#light
namespace Tim.Lisp.Core

type Quote = class
    static member String(s) = String s
    static member Number(n) = Number n
    static member Bool(b) = Bool b
end