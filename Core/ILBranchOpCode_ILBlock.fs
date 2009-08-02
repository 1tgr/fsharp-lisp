#light
namespace Tim.Lisp.Core
open System

module ILBlock =
    type ILBranchOpCode =
                        | Beq of ILBlock * ILBlock
                        | Br of ILBlock
                        | Brtrue of ILBlock * ILBlock
                        | NoBranch
                        | Ret

    and ILBlock = 
        {
            Instructions : ILOpCode list;
            mutable Branch : ILBranchOpCode
        }

    let emit instructions =
        { Instructions = instructions; Branch = NoBranch }

    let empty () = emit [ ]

    let goto branch block =
        match block with
        | { Branch = NoBranch } ->
            block.Branch <- branch

        | _ -> raise <| InvalidOperationException("Can only change a block's branch once.")

    let beq equalBlock notEqualBlock = 
        goto (Beq (equalBlock, notEqualBlock))

    let br block = 
        goto (Br block)

    let brtrue trueBlock falseBlock = 
        goto (Brtrue (trueBlock, falseBlock))

    let ret =
        goto Ret
