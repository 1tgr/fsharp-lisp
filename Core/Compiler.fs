#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit

module Compiler =
    let rec typeOf (env : Map<string, LispVal>) = function
        | ArgRef _ -> typeof<int>
        | Atom a -> env.[a] |> typeOf env
        | Bool _ -> typeof<bool>
        | IfPrimitive (_, thenValue, elseValue) ->
            match typeOf env thenValue with
            | t when t = typeOf env elseValue -> t
            | _ -> failwith "expected 'then' and 'else' branches to have same type"
        | LambdaDef (_, body) -> typeOf env body
        | LambdaRef methodBuilder -> methodBuilder.ReturnType
        | List (Atom a :: _) -> env.[a] |> typeOf env
        | List (fn :: args) -> failwith ("can't invoke " + any_to_string fn)
        | List [ ] -> failwith ("can't compile empty list")
        | ListPrimitive _ -> typeof<int>
        | Number _ -> typeof<int>
        | String _ -> typeof<string>
        | UnaryPrimitive (Eval, _) -> typeof<obj>
        | UnaryPrimitive (Quote, _) -> typeof<LispVal>
        | VariableDef _ -> typeof<Void>
        | VariableRef local -> local.LocalType

    let rec compile (generator : ILGenerator) (declaringType : TypeBuilder) =
        let rec compile' env =
            let coerce env x = 
                let env' = compile' env x
                match typeOf env x with
                    | t when t = typeof<obj> -> generator.Emit(OpCodes.Call, typeof<Convert>.GetMethod("ToInt32", [| typeof<obj> |]))
                    | t when t = typeof<int> -> ()
                    | t -> failwith ("expected int, got " + t.Name)
                env'

            let compileBinary env op = function
                | (first :: rest) ->
                    let opCode = 
                        match op with
                        | Add -> OpCodes.Add
                        | Subtract -> OpCodes.Sub
                        | Multiply -> OpCodes.Mul
                        | Divide -> OpCodes.Div
                        | Equal -> OpCodes.Ceq

                    let compileBinary' env arg =
                        let env' = coerce env arg
                        generator.Emit opCode
                        env'

                    let env' = coerce env first
                    rest |> List.fold_left compileBinary' env'
                | l -> failwith ("cannot compile list " + any_to_string l)

            let compileEval env v = 
                compile' env v |> ignore
                generator.Emit(OpCodes.Call, typeof<Eval>.GetMethod("Eval", [| typeof<LispVal> |]))

            let compileQuote = function
                | String s ->
                    generator.Emit(OpCodes.Ldstr, s)
                    generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("String"))
                | Number n ->
                    generator.Emit(OpCodes.Ldc_I4, n)
                    generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Number"))
                | Bool b ->
                    generator.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
                    generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Bool"))
                | l -> failwith ("cannot quote list " + any_to_string l)

            function
            | ArgRef index -> 
                generator.Emit(OpCodes.Ldarg, index)
                env
            | Atom a -> env.[a] |> compile' env
            | Bool b -> 
                generator.Emit (if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
                env
            | IfPrimitive (testValue, thenValue, elseValue) ->
                let env' = testValue |> compile' env
                let elseLabel = generator.DefineLabel()
                let endLabel = generator.DefineLabel()
                generator.Emit(OpCodes.Brfalse, elseLabel)
                thenValue |> compile' env' |> ignore
                generator.Emit(OpCodes.Br, endLabel)
                generator.MarkLabel elseLabel
                elseValue |> compile' env' |> ignore
                generator.MarkLabel endLabel
                env'
            | LambdaDef _ -> failwith "didn't expect lambda outside variable"
            | LambdaRef _ -> failwith "cannot compile lambda"
            | List (Atom a :: args) ->
                match env.[a] with
                | LambdaRef methodBuilder -> 
                    let env' = args |> List.fold_left compile' env
                    generator.Emit(OpCodes.Call, methodBuilder)
                    env'
                | v -> failwith ("can't invoke " + any_to_string v)
            | List (fn :: args) -> failwith ("can't invoke " + any_to_string fn)
            | List [ ] -> failwith ("can't compile empty list")
            | ListPrimitive (op, args) -> compileBinary env op args
            | Number n -> 
                generator.Emit(OpCodes.Ldc_I4, n)
                env
            | String s -> 
                generator.Emit(OpCodes.Ldstr, s)
                env
            | UnaryPrimitive (Eval, arg) -> 
                compileEval env arg
                env
            | UnaryPrimitive (Quote, arg) -> 
                compileQuote arg
                env
            | VariableDef (name, value) ->
                match value with
                | LambdaDef (names, body) ->
                    let methodBuilder = declaringType.DefineMethod(name, MethodAttributes.Static ||| MethodAttributes.Private, typeOf env body, (Array.create (List.length names) (typeof<int>)))
                    let env' = Map.add name (LambdaRef methodBuilder) env
                    let lambdaGenerator = methodBuilder.GetILGenerator()
                    let (lambdaEnv, _) = names |> List.fold_left (fun (env, index) name -> (Map.add name (ArgRef index) env, index + 1)) (env', 0)
                    body |> compile lambdaGenerator declaringType lambdaEnv |> ignore
                    lambdaGenerator.Emit(OpCodes.Ret)
                    env'
                | _ ->
                    let local = generator.DeclareLocal(typeOf env value)
                    let env' = Map.add name (VariableRef local) env
                    compile' env' value |> ignore
                    generator.Emit(OpCodes.Stloc, local)
                    env'
            | VariableRef local -> 
                generator.Emit(OpCodes.Ldloc, local)
                env

        compile'