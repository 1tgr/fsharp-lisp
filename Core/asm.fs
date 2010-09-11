#light
namespace Tim.Lisp.Core

open System
open System.Reflection
open System.Reflection.Emit

module Asm =
    open Scoped
    open Syntax

    type Asm<'a> =
        {
            OpCode : OpCode
            Operand : obj option
            ResultType : Type
            Stack : 'a list
        }

    let tryParseAsm (env : Env<_, _>) (expr : Expr) : Asm<Expr> option =
        let getMethod (name : string) (argTypes : Type list) : MethodInfo =
            let typeName, methodName =
                match name.LastIndexOf('.') with
                | -1 -> failwith "expected type.method"
                | index -> name.Substring(0, index), name.Substring(index + 1)

            let getGenericArgs (assignments : Map<string, Type> option) (expectedType : Type) (actual : ParameterInfo) : Map<string, Type> option =
                let actualType = actual.ParameterType

                if actualType.IsGenericParameter then
                    match assignments with
                    | None -> None
                    | Some map ->
                        match Map.tryFind actualType.Name map with
                        | None -> Some (Map.add actualType.Name expectedType map)
                        | Some assignedType when expectedType = assignedType -> Some map
                        | Some _ -> None

                else if actualType = expectedType then
                    assignments

                else
                    None

            let filterMethod : (MethodInfo -> MethodInfo option) =
                let argTypes = Array.ofList argTypes
                fun mi ->
                    if mi.Name = methodName then
                        let parameters = mi.GetParameters()
                        if argTypes.Length = parameters.Length then
                            match Array.fold2 getGenericArgs (Some Map.empty) argTypes parameters with
                            | None ->
                                None

                            | Some assignments when mi.IsGenericMethodDefinition ->
                                mi.GetGenericArguments()
                                |> Array.map (fun t -> assignments.[t.Name])
                                |> fun a -> mi.MakeGenericMethod(a)
                                |> Some

                            | _ ->
                                Some mi
                        else
                            None
                    else
                        None

            let t = getType env typeName
            let methods = Array.choose filterMethod <| t.GetMethods()
            if methods.Length = 0 then
                failwithf "no method on %s matching %s %A" t.FullName methodName argTypes
            else
                methods.[0]

        let parseOperand (opCode : OpCode) (operands : Expr list) : obj option =
            match opCode.OperandType with
            | OperandType.InlineMethod ->
                match operands with
                | Atom(_, name) :: types ->
                    let mi =
                        types
                        |> List.map (function
                            | Atom(_, name) -> getType env name
                            | o -> failwithf "expected a type name, not %A" o)
                        |> getMethod name

                    Some <| box mi

                | o -> failwithf "expected a method name, not %A" o

            | OperandType.InlineI ->
                match operands with
                | [Int(_, n)] -> Some <| box n
                | o -> failwithf "expected an integer, not %A" o

            | OperandType.InlineI8 ->
                match operands with
                | [Int(_, n)] -> Some <| box (byte n)
                | o -> failwithf "expected an integer, not %A" o

            | OperandType.InlineR ->
                match operands with
                | [Float(_, n)] -> Some <| box n
                | [Int(_, n)] -> Some <| box (float n)
                | o -> failwithf "expected a number, not %A" o

            | OperandType.InlineNone ->
                match operands with
                | [] -> None
                | o -> failwithf "didn't expect operand %A" o

            | OperandType.InlineString ->
                match operands with
                | [String(_, s)] -> Some <| box s
                | o -> failwithf "expected a string, not %A" o

            | OperandType.InlineType ->
                match operands with
                | [Atom(_, name)] ->
                    let t = getType env name
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

        let makeAsm (opCodeName : string) (operands : Expr list) (resultTypeName : string) (stack : Expr list) : Asm<Expr> =
            let fieldInfo = 
                typeof<OpCodes>.GetField(
                    opCodeName.Replace(".", "_"), 
                    BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.IgnoreCase)

            if fieldInfo = null then
                failwithf "invalid opcode %s" opCodeName

            let opCode : OpCode = unbox <| fieldInfo.GetValue(null)

            {
                OpCode = opCode
                Operand = parseOperand opCode operands
                ResultType = getType env resultTypeName
                Stack = stack
            }

        match expr with
        | List(_, Atom(_, ".asm") :: Atom(_, opCodeName) :: Atom(_, resultTypeName) :: stack) ->
            Some <| makeAsm opCodeName [] resultTypeName stack

        | List(_, Atom(_, ".asm") :: List(_, Atom(_, opCodeName) :: operands) :: Atom(_, resultTypeName) :: stack) ->
            Some <| makeAsm opCodeName operands resultTypeName stack

        | _ ->
            None
