#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit
open MaybeBuilder

module CodeGenerator =
    let extractAtom = function
        | Atom a -> a
        | v -> raise <| Compiler(sprintf "expected atom, got %A" v)

    let rec insertPrimitives = function
        | List (Atom "+" :: args) -> ListPrimitive (Add, args |> List.map insertPrimitives)
        | List (Atom "-" :: args) -> ListPrimitive (Subtract, args |> List.map insertPrimitives)
        | List (Atom "*" :: args) -> ListPrimitive (Multiply, args |> List.map insertPrimitives)
        | List (Atom "/" :: args) -> ListPrimitive (Divide, args |> List.map insertPrimitives)
        | List (Atom "=" :: args) -> ListPrimitive (Equal, args |> List.map insertPrimitives)
        | List (Atom "define" :: args) ->
            match args with
            | [ Atom name; v ] -> VariableDef (name, insertPrimitives v)
            | [ List (Atom name :: names); body ] -> VariableDef (name, LambdaDef (names |> List.map extractAtom, insertPrimitives body))
            | _ -> raise <| Compiler "expected define name value"

        | List (Atom "if" :: args) ->
            match args with
            | [ testValue; thenValue; elseValue ] -> IfPrimitive (insertPrimitives testValue, insertPrimitives thenValue, insertPrimitives elseValue)
            | _ -> raise <| Compiler "expected three items for if"

        | List (Atom "quote" :: args) -> 
            match args with
            | [ v ] -> QuotePrimitive v
            | _ -> raise <| Compiler "expected one item for quote"

        | List (Atom "lambda" :: args) ->
            match args with
            | [ List names; body ] -> LambdaDef (names |> List.map extractAtom, insertPrimitives body)
            | _ -> raise <| Compiler "expected lambda names body"

        | List l -> l |> List.map insertPrimitives |> List
        | v -> v

    let usingNamespaces = [ ""; "System"; "System.Diagnostics"; "System.Windows.Forms" ]
    let referencedAssemblies = 
        [
            "mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"; 
            "System, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"; 
            "System.Windows.Forms, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
        ]
        |> List.map Assembly.Load

    let ident env (a : string) =
        match Map.tryFind a env with
        | Some v -> v
        | None -> raise <| Compiler(sprintf "undeclared identifier %s" a)

    let isParamArray (parameterInfo : #ParameterInfo) = parameterInfo.IsDefined(typeof<ParamArrayAttribute>, true)
    let makeLambdaRef (methodInfo : #MethodInfo) =
        let parameters = methodInfo.GetParameters()
        let parameterTypes = parameters |> List.of_array |> List.map (fun  p -> p.ParameterType)
        let isParamArray = parameters.Length > 0 && isParamArray parameters.[parameters.Length - 1]
        LambdaRef (methodInfo, isParamArray, parameterTypes)

    let rec typeOf (env : Map<string, LispVal>) = function
        | ArgRef _ -> typeof<int>
        | Atom a -> a |> ident env |> typeOf env
        | Bool _ -> typeof<bool>
        | IfPrimitive (_, thenValue, elseValue) ->
            match typeOf env thenValue with
            | t when t = typeOf env elseValue -> t
            | _ -> raise <| Compiler("expected 'then' and 'else' branches to have same type")

        | LambdaDef (_, body) -> typeOf env body
        | LambdaRef (methodBuilder, _, _) -> methodBuilder.ReturnType
        | List (Atom a :: args) -> a |> lambdaIdent args env |> typeOf env
        | List (fn :: _) -> raise <| Compiler(sprintf "can't invoke %A" fn)
        | List [ ] -> raise <| Compiler("can't compile empty list")
        | ListPrimitive _ -> typeof<int>
        | Number _ -> typeof<int>
        | String _ -> typeof<string>
        | QuotePrimitive _ -> typeof<LispVal>
        | VariableDef _ -> typeof<Void>
        | VariableRef local -> local.LocalType

    and lambdaIdent args env (a : string) =
        let envMatches = 
            maybe {
                let! v = Map.tryFind a env
                let! r =
                    match v with
                    | LambdaRef _ -> Some v
                    | _ -> None
                return r
            } |> Option.to_list

        let clrTypeAndMethodName = 
            maybe {
                let! (typeName, methodName) = 
                    match a.LastIndexOf('.') with
                    | -1 -> None
                    | n -> Some (a.Substring(0, n), a.Substring(n + 1))

                let! clrType =
                    referencedAssemblies
                    |> List.map (fun assembly -> 
                        usingNamespaces 
                        |> List.map (fun usingNamespace -> (assembly, usingNamespace)))
                    |> List.concat
                    |> List.tryPick (fun (assembly, usingNamespace) -> option_of_nullable <| assembly.GetType(usingNamespace + "." + typeName))

                return (clrType, methodName)
            }

        let clrMatches =
            match clrTypeAndMethodName with
            | Some (clrType, methodName) -> 
                clrType.GetMethods(BindingFlags.Public ||| BindingFlags.Static) 
                |> List.of_array
                |> List.filter (fun m -> m.Name = methodName)
                |> List.map makeLambdaRef

            | None -> 
                [ ]

        let argsMatchParameters = function
            | LambdaRef (_, isParamArray, parameterTypes) ->
                let rec argsMatchParameters' argTypes (parameterTypes : #Type list) =
                    match argTypes with
                    | [ ] ->
                        match parameterTypes with
                        | [ ] -> true
                        | [ _ ] when isParamArray -> true
                        | _ -> false
                    | argType :: otherArgTypes ->
                        match parameterTypes with
                        | [ ] -> false
                        | parameterType :: otherParameterTypes -> 
                            if parameterType.IsAssignableFrom(argType)
                            then argsMatchParameters' otherArgTypes otherParameterTypes
                            else false

                argsMatchParameters' (List.map (typeOf env) args) parameterTypes

            | _ -> false

        let candidates = List.append envMatches clrMatches
        match candidates with
        | [ ] -> raise <| Compiler(sprintf "no method called %s" a)
        | _ -> ()

        let allMatches = List.filter argsMatchParameters candidates
        match allMatches with
        | [ ] -> raise <| Compiler(sprintf "no overload of %s is compatible with %A" a args)
        | firstMatch :: _ -> firstMatch

    let rec compile (generator : ILGenerator) defineMethod =
        let rec compile' env =
            let emitIf opCode env thenValue elseValue =
                let thenLabel = generator.DefineLabel()
                let endLabel = generator.DefineLabel()
                generator.Emit(opCode, thenLabel)
                elseValue |> compile' env |> ignore
                generator.Emit(OpCodes.Br, endLabel)
                generator.MarkLabel thenLabel
                thenValue |> compile' env |> ignore
                generator.MarkLabel endLabel

            function
            | ArgRef index -> 
                generator.Emit(OpCodes.Ldarg, index)
                env

            | Atom a -> 
                a |> ident env |> compile' env

            | Bool b -> 
                generator.Emit (if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
                env

            | IfPrimitive (ListPrimitive (Equal, [ a; b ]), thenValue, elseValue) ->
                let env' = a |> compile' env
                let env'' = b |> compile' env'
                emitIf OpCodes.Beq env'' thenValue elseValue
                env''

            | IfPrimitive (testValue, thenValue, elseValue) ->
                let env' = testValue |> compile' env
                emitIf OpCodes.Brtrue env' thenValue elseValue
                env'

            | LambdaDef _ -> 
                raise <| new NotImplementedException("didn't expect lambda outside variable")

            | LambdaRef _ -> 
                raise <| Compiler("can't compile lambda - try invoking it instead")

            | List (Atom a :: args) ->
                match lambdaIdent args env a with
                | LambdaRef (methodInfo, isParamArray, parameterTypes) -> 
                    let emitBoxed (expectedType : #Type) env x =
                        let env' = compile' env x
                        match typeOf env x with
                        | a when not expectedType.IsValueType && a.IsValueType -> generator.Emit(OpCodes.Box, a)
                        | _ -> ()
                        env'

                    let rec emitArgs parameterTypes env args =
                        match parameterTypes with
                        | [ parameterType ] ->
                            let (arg, otherArgs) = List.hd args, List.tl args
                            if isParamArray
                            then
                                let storeElement (env, position) x =
                                    generator.Emit(OpCodes.Dup)
                                    generator.Emit(OpCodes.Ldc_I4, int position)
                                    let env' = emitBoxed parameterType env x
                                    generator.Emit(OpCodes.Stelem, (parameterType :> Type))
                                    (env', position + 1)

                                generator.Emit(OpCodes.Ldc_I4, List.length args)
                                generator.Emit(OpCodes.Newarr, (parameterType :> Type))
                                args |> List.fold storeElement (env, 0) |> fst
                            else
                                emitArgs [ ] (emitBoxed parameterType env arg) otherArgs

                        | parameterType :: otherParameterTypes ->
                            let (arg, otherArgs) = List.hd args, List.tl args
                            emitArgs otherParameterTypes (emitBoxed parameterType env arg) otherArgs

                        | [ ] -> 
                            env

                    let env' = args |> emitArgs parameterTypes env
                    generator.Emit(OpCodes.Call, methodInfo)
                    env'

                | v -> raise <| new NotImplementedException(sprintf "can't invoke variable %A" v)

            | List (fn :: args) -> 
                raise <| new NotImplementedException(sprintf "can't invoke value %A" fn)

            | List [ ] -> 
                raise <| Compiler("can't invoke empty list")

            | ListPrimitive (op, args) -> 
                match args with
                | arg :: otherArgs ->
                    let opCode = 
                        match op with
                        | Add -> OpCodes.Add
                        | Subtract -> OpCodes.Sub
                        | Multiply -> OpCodes.Mul
                        | Divide -> OpCodes.Div
                        | Equal -> OpCodes.Ceq

                    let coerceToInt env x = 
                        let env' = compile' env x
                        match typeOf env x with
                        | t when t = typeof<obj> -> generator.Emit(OpCodes.Call, typeof<Convert>.GetMethod("ToInt32", [| typeof<obj> |]))
                        | t when t = typeof<int> -> ()
                        | t -> raise <| new NotImplementedException("expected int, got " + t.Name)
                        env'

                    let emitBinaryOp env arg =
                        let env' = coerceToInt env arg
                        generator.Emit opCode
                        env'

                    let env' = coerceToInt env arg
                    otherArgs |> List.fold emitBinaryOp env'

                | l -> 
                    raise <| Compiler(sprintf "cannot compile list %A" l)

            | Number n -> 
                generator.Emit(OpCodes.Ldc_I4, n)
                env

            | String s -> 
                generator.Emit(OpCodes.Ldstr, s)
                env

            | QuotePrimitive (Bool b) -> 
                generator.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
                generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Bool"))
                env

            | QuotePrimitive (Number n) -> 
                generator.Emit(OpCodes.Ldc_I4, n)
                generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("Number"))
                env

            | QuotePrimitive (String s) -> 
                generator.Emit(OpCodes.Ldstr, s)
                generator.Emit(OpCodes.Call, typeof<Quote>.GetMethod("String"))
                env

            | QuotePrimitive v -> 
                raise <| new NotImplementedException(sprintf "cannot quote %A yet" v)

            | VariableDef (name, value) ->
                match value with
                | LambdaDef (paramNames, body) ->
                    let (lambdaInfo, lambdaGenerator) = 
                        defineMethod
                            name
                            (typeOf env body)
                            (List.replicate (List.length paramNames) (typeof<int>))

                    let envWithLambda = 
                        env 
                        |> (LambdaRef (lambdaInfo, false, (List.map (fun _ -> typeof<int>) paramNames)) 
                            |> Map.add name)

                    let (envWithLambdaArgs, _) = 
                        paramNames 
                        |> ((envWithLambda, 0) 
                            |> List.fold (fun (env, index) name -> (Map.add name (ArgRef index) env, index + 1)))

                    body |> compile lambdaGenerator defineMethod envWithLambdaArgs |> ignore
                    lambdaGenerator.Emit(OpCodes.Ret)
                    envWithLambda

                | _ ->
                    let local = generator.DeclareLocal(typeOf env value)
                    let envWithVariable = Map.add name (VariableRef local) env
                    compile' envWithVariable value |> ignore
                    generator.Emit(OpCodes.Stloc, local)
                    envWithVariable

            | VariableRef local -> 
                generator.Emit(OpCodes.Ldloc, local)
                env

        compile'