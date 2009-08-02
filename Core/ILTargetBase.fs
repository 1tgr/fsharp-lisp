#light
namespace Tim.Lisp.Core
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open ILBlock

[<AbstractClass>]
type ILTargetBase(generator : ILGenerator, methodInfo : MethodInfo) =
    let targets =
        function
        | Beq (equalTarget, notEqualTarget) ->
            [ notEqualTarget; equalTarget ]

        | Br target ->
            [ target ]

        | Brtrue (trueTarget, falseTarget) ->
            [ falseTarget; trueTarget ]
            
        | NoBranch ->
            raise <| new InvalidOperationException("Didn't expect NoBranch block.")

        | Ret -> 
            [ ]

    let rec visitBlocks (visitedBlocks : Dictionary<ILBlock, bool>) ({ Branch = branch } as block) =
        if visitedBlocks.ContainsKey(block)
        then [ ]
        else
            visitedBlocks.Add(block, true)
            let otherBlocks = 
                branch
                |> targets
                |> List.collect (visitBlocks visitedBlocks)

            block :: otherBlocks

    let rec defineLabels (labels : Dictionary<ILBlock, Label>) =
        function
        | block :: otherBlocks ->
            let label = generator.DefineLabel()
            labels.Add(block, label)
            defineLabels labels otherBlocks

        | [ ] ->
            ()

    let emitInstruction =
        function
        | Add -> generator.Emit(OpCodes.Add)
        | Box boxType -> generator.Emit(OpCodes.Box, boxType)
        | Call methodInfo -> generator.Emit(OpCodes.Call, methodInfo)
        | Ceq -> generator.Emit(OpCodes.Ceq)
        | Div -> generator.Emit(OpCodes.Div)
        | Dup -> generator.Emit(OpCodes.Dup)
        | Ldarg index -> generator.Emit(OpCodes.Ldarg, index)
        | Ldc_I4 n -> generator.Emit(OpCodes.Ldc_I4, n)
        | Ldc_I4_0 -> generator.Emit(OpCodes.Ldc_I4_0)
        | Ldc_I4_1 -> generator.Emit(OpCodes.Ldc_I4_1)
        | Ldloc local -> generator.Emit(OpCodes.Ldloc, local)
        | Ldstr s -> generator.Emit(OpCodes.Ldstr, s)
        | Mul -> generator.Emit(OpCodes.Mul)
        | Newarr elementType -> generator.Emit(OpCodes.Newarr, elementType)
        | Pop -> generator.Emit(OpCodes.Pop)
        | Stelem elementType -> generator.Emit(OpCodes.Stelem, elementType)
        | Stloc local -> generator.Emit(OpCodes.Stloc, local)
        | Sub -> generator.Emit(OpCodes.Sub)

    let rec emitInstructions block = 
        let isRetBlock =
            match block with
            | { Branch = Ret } -> true
            | { Branch = Br { Instructions = [ ]; Branch = Ret } } -> true
            | _ -> false

        function
        | [ Call _ as instruction ] when isRetBlock ->
            generator.Emit(OpCodes.Tailcall)
            emitInstruction instruction

        | instruction :: otherInstructions ->
            emitInstruction instruction
            emitInstructions block otherInstructions

        | [ ] ->
            ()

    let emitBranch (labels : Dictionary<ILBlock, Label>) =
        function
        | Beq (equalBlock, notEqualBlock) ->
            generator.Emit(OpCodes.Beq, labels.[equalBlock])
            generator.Emit(OpCodes.Br, labels.[notEqualBlock])

        | Br block ->
            generator.Emit(OpCodes.Br, labels.[block])

        | Brtrue (trueBlock, falseBlock) ->
            generator.Emit(OpCodes.Brtrue, labels.[trueBlock])
            generator.Emit(OpCodes.Br, labels.[falseBlock])

        | NoBranch ->
            raise <| new InvalidOperationException("Didn't expect NoBranch block.")

        | Ret -> 
            generator.Emit(OpCodes.Ret)

    let rec emitBlocks (labels : Dictionary<ILBlock, Label>) =
        function
        | { Instructions = instructions; Branch = branch } as block :: otherBlocks ->
            let label = labels.[block]
            generator.MarkLabel(label)

            emitInstructions block instructions

            match branch, otherBlocks with
            | Br target, next :: _ when target = next ->
                ()

            | Br { Instructions = [ ]; Branch = Ret }, _->
                generator.Emit(OpCodes.Ret)

            | Beq (equalTarget, notEqualTarget), next :: _ when equalTarget = next ->
                generator.Emit(OpCodes.Bne_Un, labels.[notEqualTarget])

            | Beq (equalTarget, notEqualTarget), next :: _ when notEqualTarget = next ->
                generator.Emit(OpCodes.Beq, labels.[equalTarget])

            | branchOpCode, _ ->
                emitBranch labels branchOpCode

            emitBlocks labels otherBlocks

        | [ ] ->
            ()

    abstract member DefineMethodCore : string -> Type -> Type list -> IILTarget

    interface IILTarget with
        member this.DefineMethod name returnType parameterTypes = 
            this.DefineMethodCore name returnType parameterTypes

        member this.GenerateIL block = 
            let visitedBlocks = new Dictionary<ILBlock, bool>()
            let blocks = visitBlocks visitedBlocks block

            let labels = new Dictionary<ILBlock, Label>()
            defineLabels labels blocks
            emitBlocks labels blocks

        member this.DeclareLocal localType = 
            generator.DeclareLocal(localType)

        member this.MethodInfo 
            with get() = methodInfo
    end
