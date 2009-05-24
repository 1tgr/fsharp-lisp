#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit
open MaybeBuilderModule

module CodeGenerator =
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
        | None -> failwith <| "undeclared identifier " + a

    let isParamArray (parameterInfo : #ParameterInfo) = parameterInfo.IsDefined(typeof<ParamArrayAttribute>, true)
    let makeLambdaRef (methodInfo : #MethodInfo) =
        let parameters = methodInfo.GetParameters()
        let parameterTypes = parameters |> List.of_array |> List.map (fun  p -> p.ParameterType)
        let isParamArray = parameters.Length > 0 && isParamArray parameters.[parameters.Length - 1]
        LambdaRef (methodInfo, isParamArray, parameterTypes)

    let rec methodMatch' isParamArray argTypes (parameterTypes : Type list) =
        match argTypes with
        | [ ] ->
            match parameterTypes with
            | [ ] -> true
            | [ parameterArrayType ] when isParamArray -> true
            | _ -> false
        | argType :: otherArgTypes ->
            match parameterTypes with
            | [ ] -> false
            | parameterType :: otherParameterTypes -> 
                if parameterType.IsAssignableFrom(argType)
                then methodMatch' isParamArray otherArgTypes otherParameterTypes
                else false

    let rec typeOf (env : Map<string, LispVal>) = function
        | ArgRef _ -> typeof<int>
        | Atom a -> a |> ident env |> typeOf env
        | Bool _ -> typeof<bool>
        | IfPrimitive (_, thenValue, elseValue) ->
            match typeOf env thenValue with
            | t when t = typeOf env elseValue -> t
            | _ -> failwith "expected 'then' and 'else' branches to have same type"
        | LambdaDef (_, body) -> typeOf env body
        | LambdaRef (methodBuilder, _, _) -> methodBuilder.ReturnType
        | List (Atom a :: args) -> a |> lambdaIdent args env |> typeOf env
        | List (fn :: _) -> failwith <| sprintf "can't invoke %A" fn
        | List [ ] -> failwith "can't compile empty list"
        | ListPrimitive _ -> typeof<int>
        | Number _ -> typeof<int>
        | String _ -> typeof<string>
        | UnaryPrimitive (Eval, _) -> typeof<obj>
        | UnaryPrimitive (Quote, _) -> typeof<LispVal>
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
            | None -> [ ]

        let methodMatch env args = function
            | LambdaRef (_, isParamArray, parameterTypes) ->
                let argTypes = args |> List.map (typeOf env)
                methodMatch' isParamArray argTypes parameterTypes
            | v -> failwith <| sprintf "methodMatch didn't expect %A" v

        let candidates = List.append envMatches clrMatches
        match candidates with
        | [ ] -> failwith <| sprintf "no method called %s" a
        | _ -> ()

        let allMatches = List.filter (methodMatch env args) candidates
        match allMatches with
        | [ ] -> failwith <| sprintf "no overload of %s is compatible with %A" a args
        | firstMatch :: _ -> firstMatch

    let rec compile (generator : ILGenerator) (declaringType : TypeBuilder) =
        let rec compile' env =
            let coerce env x = 
                let env' = compile' env x
                match typeOf env x with
                    | t when t = typeof<obj> -> generator.Emit(OpCodes.Call, typeof<Convert>.GetMethod("ToInt32", [| typeof<obj> |]))
                    | t when t = typeof<int> -> ()
                    | t -> failwith <| "expected int, got " + t.Name
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
                    rest |> List.fold compileBinary' env'
                | l -> failwith <| sprintf "cannot compile list %A" l

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
                | l -> failwith <| sprintf "cannot quote list %A" l

            let compileIf opCode env thenValue elseValue =
                let thenLabel = generator.DefineLabel()
                let endLabel = generator.DefineLabel()
                generator.Emit(opCode, thenLabel)
                elseValue |> compile' env |> ignore
                generator.Emit(OpCodes.Br, endLabel)
                generator.MarkLabel thenLabel
                thenValue |> compile' env |> ignore
                generator.MarkLabel endLabel

            let compileBoxed (expectedType : #Type) env x =
                let env' = compile' env x
                match typeOf env x with
                | a when not expectedType.IsValueType && a.IsValueType -> generator.Emit(OpCodes.Box, a)
                | _ -> ()
                env'

            let rec makeArgs isParamArray = function
                | [ parameterType ] ->
                    fun env ->
                        function 
                        | [ ] -> failwith "provided 1 too few args"
                        | (arg :: resta) as args ->
                            if isParamArray
                            then
                                let storeElement (env, position) x =
                                    generator.Emit(OpCodes.Dup)
                                    generator.Emit(OpCodes.Ldc_I4, int position)
                                    let env' = compileBoxed parameterType env x
                                    generator.Emit(OpCodes.Stelem, (parameterType :> Type))
                                    (env', position + 1)

                                generator.Emit(OpCodes.Ldc_I4, List.length args)
                                generator.Emit(OpCodes.Newarr, (parameterType :> Type))
                                args |> List.fold storeElement (env, 0) |> fst
                            else
                                let env' = compileBoxed parameterType env arg
                                makeArgs false [ ] env' resta
                | (parameterType :: restp) as parameterTypes ->
                    fun env ->
                        function 
                        | [ ] -> failwith <| "provided " + string (List.length parameterTypes) + " too few args"
                        | (arg :: resta) as args ->
                            let env' = compileBoxed parameterType env arg
                            makeArgs isParamArray restp env' resta
                | [ ] -> 
                    fun env -> 
                        function
                        | [ ] -> env
                        | args -> failwith <| "provided " + string (List.length args) + " too many args"

            function
            | ArgRef index -> 
                generator.Emit(OpCodes.Ldarg, index)
                env
            | Atom a -> a |> ident env |> compile' env
            | Bool b -> 
                generator.Emit (if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
                env
            | IfPrimitive (ListPrimitive (Equal, [ a; b ]), thenValue, elseValue) ->
                let env' = a |> compile' env
                let env'' = b |> compile' env'
                compileIf OpCodes.Beq env'' thenValue elseValue
                env''
            | IfPrimitive (testValue, thenValue, elseValue) ->
                let env' = testValue |> compile' env
                compileIf OpCodes.Brtrue env' thenValue elseValue
                env'
            | LambdaDef _ -> failwith "didn't expect lambda outside variable"
            | LambdaRef _ -> failwith "cannot compile lambda"
            | List (Atom a :: args) ->
                match lambdaIdent args env a with
                | LambdaRef (methodInfo, isParamArray, parameterTypes) -> 
                    let env' = args |> makeArgs isParamArray parameterTypes env
                    generator.Emit(OpCodes.Call, methodInfo)
                    env'
                | v -> failwith <| sprintf "can't invoke variable %A" v
            | List (fn :: args) -> failwith <| sprintf "can't invoke value %A" fn
            | List [ ] -> failwith "can't compile empty list"
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
                | LambdaDef (paramNames, body) ->
                    let methodBuilder = 
                        declaringType.DefineMethod(
                            name, 
                            MethodAttributes.Static ||| MethodAttributes.Private, 
                            typeOf env body, 
                            Array.create (List.length paramNames) (typeof<int>))

                    paramNames
                    |> List.iteri (fun position name -> methodBuilder.DefineParameter(position + 1, ParameterAttributes.None, name) |> ignore)

                    let envWithLambda = 
                        env 
                        |> (LambdaRef (methodBuilder, false, (List.map (fun _ -> typeof<int>) paramNames)) 
                            |> Map.add name)

                    let (envWithLambdaArgs, _) = 
                        paramNames 
                        |> ((envWithLambda, 0) 
                            |> List.fold (fun (env, index) name -> (Map.add name (ArgRef index) env, index + 1)))

                    let lambdaGenerator = methodBuilder.GetILGenerator()
                    body |> compile lambdaGenerator declaringType envWithLambdaArgs |> ignore
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