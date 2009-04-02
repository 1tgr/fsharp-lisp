#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit

module Compiler =
    let rec typeOf (env : Map<string, LispVal>) = function
        | Atom a -> env.[a] |> typeOf env
        | Bool _ -> typeof<bool>
        | CompiledLambda methodBuilder -> methodBuilder.ReturnType
        | CompiledVariable local -> local.LocalType
        | LambdaPrimitive (_, body) -> typeOf env body
        | List (Atom a :: _) -> env.[a] |> typeOf env
        | List (fn :: args) -> failwith ("can't invoke " + any_to_string fn)
        | List [ ] -> failwith ("can't compile empty list")
        | ListPrimitive _ -> typeof<int>
        | Number _ -> typeof<int>
        | String _ -> typeof<string>
        | UnaryPrimitive (Eval, _) -> typeof<obj>
        | UnaryPrimitive (Quote, _) -> typeof<LispVal>
        | VariablePrimitive (_, v) -> typeOf env v

    let rec compile (generator : ILGenerator) (declaringType : TypeBuilder) initialEnv x =
        let rec compile1 (env : Map<string, LispVal>) = function
            | Atom a -> env.[a] |> compile1 env
            | Bool _ -> env
            | CompiledLambda _ -> env
            | CompiledVariable _ -> env
            | LambdaPrimitive (_, body) -> failwith "didn't expect lambda outside variable"
            | ListPrimitive _ -> env
            | UnaryPrimitive (Eval, _) -> env
            | UnaryPrimitive (Quote, _) -> env
            | List (Atom a :: args) -> env
            | List (fn :: args) -> failwith ("can't invoke " + any_to_string fn)
            | List [ ] -> failwith ("can't compile empty list")
            | Number _ -> env
            | String _ -> env
            | VariablePrimitive (name, LambdaPrimitive (_, body)) ->
                let methodBuilder = declaringType.DefineMethod(name, MethodAttributes.Static ||| MethodAttributes.Private, typeOf env body, [| |])
                body |> compile1 env |> Map.add name (CompiledLambda methodBuilder)
            | VariablePrimitive (name, value) ->
                let local = generator.DeclareLocal(typeOf env value)
                value |> compile1 env |> Map.add name (CompiledVariable local)

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
            | CompiledLambda _ -> failwith "didn't expect lambda outside variable"
            | CompiledVariable local -> generator.Emit(OpCodes.Ldloc, local)
            | LambdaPrimitive _ -> failwith "cannot compile lambda"
            | List (Atom a :: args) ->
                match env.[a] with
                | CompiledLambda methodBuilder -> generator.Emit(OpCodes.Call, methodBuilder)
                | v -> failwith ("can't invoke " + any_to_string v)
            | List (fn :: args) -> failwith ("can't invoke " + any_to_string fn)
            | List [ ] -> failwith ("can't compile empty list")
            | ListPrimitive (op, args) -> compileBinary op args
            | Number n -> generator.Emit(OpCodes.Ldc_I4, n)
            | String s -> generator.Emit(OpCodes.Ldstr, s)
            | UnaryPrimitive (Eval, arg) -> compileEval arg
            | UnaryPrimitive (Quote, arg) -> compileQuote arg
            | VariablePrimitive (name, value) ->
                match env.[name] with
                | CompiledLambda methodBuilder ->
                    match value with
                    | LambdaPrimitive (_, body) ->
                        let generator' = methodBuilder.GetILGenerator()
                        compile generator' declaringType env body |> ignore
                        generator'.Emit(OpCodes.Ret)
                        generator.Emit(OpCodes.Call, methodBuilder)
                    | v -> failwith ("expected lambda variable, got " + any_to_string v)
                | CompiledVariable local ->
                    compile2 value
                    generator.Emit(OpCodes.Stloc, local)
                    generator.Emit(OpCodes.Ldloc, local)
                | v -> failwith ("didn't expect " + any_to_string v)

        compile2 x
        env