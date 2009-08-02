#light
namespace Tim.Lisp.Core
open System

type CompilerException(message) = 
    inherit Exception(message)
