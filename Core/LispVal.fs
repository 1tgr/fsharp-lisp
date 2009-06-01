#light
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit

type LispVal = 
             | ArgRef of int                                // Value of function argument
             | Atom of string                               // Function or variable name
             | Bool of bool                                 // Boolean constant
             | IfPrimitive of LispVal * LispVal * LispVal   // if/then/else
             | LambdaDef of string list * LispVal           // Function declaration
             | LambdaRef of MethodInfo * bool * Type list   // Function invocation
             | List of LispVal list                         // List constant
             | ListPrimitive of ListOp * LispVal list       // Built-in list operation
             | Number of int                                // Integer constant
             | String of string                             // String constant
             | VariableDef of string * LispVal              // Variable declaration
             | VariableRef of LocalBuilder                  // Value of variable