#light
namespace Tim.Lisp.Core
open System.Reflection.Emit

type LispVal = 
             | ArgRef of int
             | Atom of string
             | Bool of bool
             | IfPrimitive of LispVal * LispVal * LispVal
             | LambdaDef of string list * LispVal
             | LambdaRef of MethodBuilder
             | List of LispVal list
             | ListPrimitive of ListOp * LispVal list
             | Number of int
             | String of string
             | UnaryPrimitive of UnaryOp * LispVal
             | VariableDef of string * LispVal
             | VariableRef of LocalBuilder