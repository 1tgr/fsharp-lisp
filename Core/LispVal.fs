#light
namespace Tim.Lisp.Core

type LispVal = Atom of string
             | List of LispVal list
             | Number of int
             | String of string
             | Bool of bool