#light
namespace Tim.Lisp.Core
open System.Reflection.Emit

module Evaluator =
    let boxUnbox<'a> fn = fun a b -> fn (unbox<'a> a) (unbox<'a> b) |> box

    let rec eval (env : Map<string, LispVal>) = 
        let evalBinary fn args = (env, args |> List.map (eval env >> snd) |> List.reduce_left (boxUnbox fn))

        let evalEval = function
            | [ v ] -> (env, v |> eval env |> snd)
            | l -> failwith ("cannot evaluate list " + any_to_string l)

        let evalQuote = function
            | [ v ] -> (env, box v)
            | l -> failwith ("cannot quote list " + any_to_string l)

        function
        | Atom a -> (env, env.[a] |> eval env |> snd)
        | Bool b as v -> (env, box b)
        | List (Atom "+" :: args) -> evalBinary (+) args
        | List (Atom "-" :: args) -> evalBinary (-) args
        | List (Atom "*" :: args) -> evalBinary (*) args
        | List (Atom "/" :: args) -> evalBinary (/) args
        | List (Atom "eval" :: args) -> evalEval args
        | List (Atom "quote" :: args) -> evalQuote args
        | List l -> failwith ("cannot evaluate list " + any_to_string l)
        | Number n as v -> (env, box n)
        | String s as v -> (env, box s)

    