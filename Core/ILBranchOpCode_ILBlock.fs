#light
namespace Tim.Lisp.Core
open System

type ILBranchOpCode =
                    | Beq of ILBlock * ILBlock
                    | Br of ILBlock
                    | Brtrue of ILBlock * ILBlock
                    | NoBranch
                    | Ret

and ILBlock() =
    let mutable instructions : ILOpCode list = List.empty
    let mutable branch = NoBranch

    member this.Emit opCode = 
        instructions <- opCode :: instructions

    member this.Instructions
        with get() = List.rev instructions

    member this.Branch
        with get() = branch
        and set(value) = 
            match branch with
            | NoBranch -> branch <- value
            | _ -> raise <| new InvalidOperationException("Can only set Branch once.")

module ILBlockModule =
    let emit (block : ILBlock) opCode = block.Emit opCode
