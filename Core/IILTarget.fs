#light
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit
open ILBlock

type IILTarget =
    abstract DefineMethod : string -> Type -> Type list -> IILTarget
    abstract GenerateIL : ILBlock -> unit
    abstract DeclareLocal : Type -> LocalBuilder
    abstract MethodInfo : unit -> MethodInfo with get
