#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.Reflection.Emit

module Compiler =
    let rec typeOf (env : Map<string, LispVal>) = function
        | Atom a -> env.[a] |> typeOf env
        | Bool _ -> typeof<bool>
        | ListPrimitive _ -> typeof<int>
        | UnaryPrimitive (Eval, _) -> typeof<obj>
        | UnaryPrimitive (Quote, _) -> typeof<LispVal>
        | List l -> failwith ("cannot evaluate list " + any_to_string l)
        | Number _ -> typeof<int>
        | String _ -> typeof<string>

    let rec compile1 (env : Map<string, LispVal>) = function
        | Atom a -> env.[a] |> compile1 env
        | Bool _ -> env
        | ListPrimitive _ -> env
        | UnaryPrimitive (Eval, _) -> env
        | UnaryPrimitive (Quote, _) -> env
        | List l -> failwith ("cannot compile list " + any_to_string l)
        | Number _ -> env
        | String _ -> env

    let compile (generator : ILGenerator) initialEnv x =
        let env = x |> compile1 initialEnv

        let rec compile2 = 
            let coerce x = 
                compile2 x
                match typeOf env x with
                    | t when t = typeof<obj> -> generator.Emit(OpCodes.Call, typeof<Convert>.GetMethod("ToInt32", [| typeof<obj> |]))
                    | t when t = typeof<int> -> ()
                    | t -> failwith ("expected int, got " + t.Name)

            let compileBinary op = function
                | (a :: rest) ->
                    let opCode = 
                        match op with
                        | Add -> OpCodes.Add
                        | Subtract -> OpCodes.Sub
                        | Multiply -> OpCodes.Mul
                        | Divide -> OpCodes.Div

                    let compile2' x =
                        coerce x
                        generator.Emit(opCode)

                    coerce a
                    rest |> List.iter compile2'
                | l -> failwith ("cannot compile list " + any_to_string l)

            let compileEval v = 
                compile2 v
                generator.Emit(OpCodes.Call, typeof<Eval>.GetMethod("Eval", [| typeof<LispVal> |]))

            let compileQuote = function
                | String s ->
                    generator.Emit(OpCodes.Ldstr, s)
                    generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("String"))
                | Number n ->
                    generator.Emit(OpCodes.Ldc_I4, n)
                    generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Number"))
                | Bool true ->
                    generator.Emit(OpCodes.Ldc_I4_1)
                    generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Bool"))
                | Bool false ->
                    generator.Emit(OpCodes.Ldc_I4_0)
                    generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Bool"))
                | l -> failwith ("cannot quote list " + any_to_string l)

            function
            | Atom a -> env.[a] |> compile2
            | Bool true -> generator.Emit(OpCodes.Ldc_I4_1)
            | Bool false -> generator.Emit(OpCodes.Ldc_I4_0)
            | ListPrimitive (op, args) -> compileBinary op args
            | UnaryPrimitive (Eval, arg) -> compileEval arg
            | UnaryPrimitive (Quote, arg) -> compileQuote arg
            | List l -> failwith ("cannot compile list " + any_to_string l)
            | Number n -> generator.Emit(OpCodes.Ldc_I4, n)
            | String s -> generator.Emit(OpCodes.Ldstr, s)

        compile2 x
        env