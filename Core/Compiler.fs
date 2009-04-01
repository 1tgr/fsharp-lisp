#light
namespace Tim.Lisp.Core
open System
open System.Reflection.Emit

module Compiler =
    let rec typeOf (env : Map<string, LispVal>) = function
        | Atom a -> env.[a] |> typeOf env
        | Bool _ -> typeof<bool>
        | List (Atom "+" :: _) | List (Atom "-" :: _) | List (Atom "*" :: _) | List (Atom "/" :: _) -> typeof<int>
        | List (Atom "eval" :: _) -> typeof<obj>
        | List (Atom "quote" :: _) -> typeof<LispVal>
        | List l -> failwith ("cannot evaluate list " + any_to_string l)
        | Number _ -> typeof<int>
        | String _ -> typeof<string>

    let rec compile1 (env : Map<string, LispVal>) = function
        | Atom a -> env.[a] |> compile1 env
        | Bool _ -> env
        | List (Atom "+" :: _) | List (Atom "-" :: _) | List (Atom "*" :: _) | List (Atom "/" :: _) -> env
        | List (Atom "quote" :: _) -> env
        | List l -> failwith ("cannot compile list " + any_to_string l)
        | Number _ -> env
        | String _ -> env

    let compileQuote (generator : ILGenerator) = function
        | [ String s ] ->
            generator.Emit(OpCodes.Ldstr, s)
            generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("String"))
        | [ Number n ] ->
            generator.Emit(OpCodes.Ldc_I4, n)
            generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Number"))
        | [ Bool true ] ->
            generator.Emit(OpCodes.Ldc_I4_1)
            generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Bool"))
        | [ Bool false ] ->
            generator.Emit(OpCodes.Ldc_I4_0)
            generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Bool"))
        | l -> failwith ("cannot quote list " + any_to_string l)

    let rec compile2 (env : Map<string, LispVal>) (generator : ILGenerator) = 
        let compileBinary opCode = function
            | (a :: rest) ->
                let coerce x = 
                    compile2 env generator x
                    match typeOf env x with
                        | t when t = typeof<obj> -> generator.Emit(OpCodes.Call, typeof<Convert>.GetMethod("ToInt32", [| typeof<obj> |]))
                        | t when t = typeof<int> -> ()
                        | t -> failwith ("expected int, got " + t.Name)

                let compile2' x =
                    coerce x
                    generator.Emit(opCode)

                coerce a
                rest |> List.iter compile2'
            | l -> failwith ("cannot compile list " + any_to_string l)

        let compileEval = function
            | [ v ] ->
                compile2 env generator v
                generator.Emit(OpCodes.Call, typeof<Eval>.GetMethod("Eval", [| typeof<LispVal> |]))
            | l -> failwith ("cannot compile list " + any_to_string l)

        function
        | Atom a -> env.[a] |> compile2 env generator
        | Bool true -> generator.Emit(OpCodes.Ldc_I4_1)
        | Bool false -> generator.Emit(OpCodes.Ldc_I4_0)
        | List (Atom "+" :: args) -> compileBinary OpCodes.Add args
        | List (Atom "-" :: args) -> compileBinary OpCodes.Sub args
        | List (Atom "*" :: args) -> compileBinary OpCodes.Mul args
        | List (Atom "/" :: args) -> compileBinary OpCodes.Div args
        | List (Atom "eval" :: args) -> compileEval args
        | List (Atom "quote" :: args) -> compileQuote generator args
        | List l -> failwith ("cannot compile list " + any_to_string l)
        | Number n -> generator.Emit(OpCodes.Ldc_I4, n)
        | String s -> generator.Emit(OpCodes.Ldstr, s)