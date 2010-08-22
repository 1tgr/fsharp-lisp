#light
namespace Tim.Lisp.Core

open System
open System.Reflection
open System.Reflection.Emit

module Asm =
    open Syntax

    type Asm<'a> =
        {
            OpCode : OpCode
            Operand : obj option
            ResultType : Type
            Stack : 'a list
        }

    let parseAsmOperand (opCode : OpCode) (operand : Expr<_> option) : obj option =
        match opCode.OperandType with
        | OperandType.InlineMethod ->
            match operand with
            | Some (Atom(_, name)) ->
                let typeName, methodName =
                    match name.LastIndexOf('.') with
                    | -1 -> failwith "expected type.method"
                    | index -> name.Substring(0, index), name.Substring(index + 1)

                let t = Type.GetType(typeName, true)
                Some <| box (t.GetMethod(methodName))

            | o -> failwithf "expected a method name, not %A" o

        | OperandType.InlineI ->
            match operand with
            | Some (Int(_, n)) -> Some <| box n
            | o -> failwithf "expected an integer, not %A" o

        | OperandType.InlineI8 ->
            match operand with
            | Some (Int(_, n)) -> Some <| box (byte n)
            | o -> failwithf "expected an integer, not %A" o

        | OperandType.InlineR ->
            match operand with
            | Some (Float(_, n)) -> Some <| box n
            | Some (Int(_, n)) -> Some <| box (float n)
            | o -> failwithf "expected a number, not %A" o

        | OperandType.InlineNone ->
            match operand with
            | Some o -> failwithf "didn't expect operand %A" o
            | None -> None

        | OperandType.InlineString ->
            match operand with
            | Some (String(_, s)) -> Some <| box s
            | o -> failwithf "expected a string, not %A" o

        | OperandType.InlineType ->
            match operand with
            | Some (Atom(_, name)) ->
                let t = Type.GetType(name, true)
                Some <| box t

            | o -> failwithf "expected a type name, not %A" o

        | OperandType.InlineBrTarget
        | OperandType.InlineField
        | OperandType.InlineSig
        | OperandType.InlineSwitch
        | OperandType.InlineTok
        | OperandType.InlineVar
        | OperandType.ShortInlineBrTarget
        | OperandType.ShortInlineI
        | OperandType.ShortInlineR
        | OperandType.ShortInlineVar
        | _ -> failwith "asm operands of type %A are not supported" opCode.OperandType

    let makeAsm (opCodeName : string) (operand : Expr<_> option) (resultTypeName : string) (stack : Expr<_> list) : Asm<Expr<_>> =
        let fieldInfo = 
            typeof<OpCodes>.GetField(
                opCodeName.Replace(".", "_"), 
                BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.IgnoreCase)

        if fieldInfo = null then
            failwithf "invalid opcode %s" opCodeName

        let opCode : OpCode = unbox <| fieldInfo.GetValue(null)

        {
            OpCode = opCode
            Operand = parseAsmOperand opCode operand
            ResultType = typeof<int>.Assembly.GetType(resultTypeName, true)
            Stack = stack
        }

    let tryParseAsm (expr : Expr<_>) : Asm<Expr<_>> option =
        match expr with
        | List(_, Atom(_, ".asm") :: Atom(_, opCodeName) :: Atom(_, resultTypeName) :: stack) ->
            Some <| makeAsm opCodeName None resultTypeName stack

        | List(_, Atom(_, ".asm") :: List(_, [Atom(_, opCodeName); operand]) :: Atom(_, resultTypeName) :: stack) ->
            Some <| makeAsm opCodeName (Some operand) resultTypeName stack

        | _ ->
            None
