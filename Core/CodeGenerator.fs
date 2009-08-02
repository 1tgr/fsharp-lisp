#light
#nowarn "40"
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit
open ILBlock
open MaybeBuilder

module CodeGenerator =
    let extractAtom = function
        | Atom a -> a
        | v -> raise <| CompilerException(sprintf "expected atom, got %A" v)

    let rec insertPrimitives = 
        function
        | List (Atom "+" :: args) -> ListPrimitive (ListOp.Add, args |> List.map insertPrimitives)
        | List (Atom "-" :: args) -> ListPrimitive (Subtract, args |> List.map insertPrimitives)
        | List (Atom "*" :: args) -> ListPrimitive (Multiply, args |> List.map insertPrimitives)
        | List (Atom "/" :: args) -> ListPrimitive (Divide, args |> List.map insertPrimitives)
        | List (Atom "=" :: args) -> ListPrimitive (Equal, args |> List.map insertPrimitives)
        | List (Atom "define" :: args) ->
            match args with
            | [ Atom name; v ] -> 
                VariableDef (name, insertPrimitives v)

            | [ List (Atom name :: names); body ] -> 
                VariableDef (name, LambdaDef (names |> List.map extractAtom, insertPrimitives body))

            | _ -> 
                raise <| CompilerException "expected define name value"

        | List (Atom "if" :: args) ->
            match args with
            | [ testValue; thenValue; elseValue ] -> 
                IfPrimitive (insertPrimitives testValue, insertPrimitives thenValue, insertPrimitives elseValue)

            | _ -> 
                raise <| CompilerException "expected three items for if"

        | List (Atom "lambda" :: args) ->
            match args with
            | [ List names; body ] -> 
                LambdaDef (names |> List.map extractAtom, insertPrimitives body)

            | _ -> 
                raise <| CompilerException "expected lambda names body"

        | List l -> 
            l |> List.map insertPrimitives |> List

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
        | None -> raise <| CompilerException(sprintf "undeclared identifier %s" a)

    let isParamArray (parameterInfo : #ParameterInfo) = parameterInfo.IsDefined(typeof<ParamArrayAttribute>, true)
    let makeLambdaRef (methodInfo : #MethodInfo) =
        let parameters = methodInfo.GetParameters()
        let parameterTypes = parameters |> List.of_array |> List.map (fun p -> p.ParameterType)
        let isParamArray = parameters.Length > 0 && isParamArray parameters.[parameters.Length - 1]
        LambdaRef (methodInfo, isParamArray, parameterTypes)

    let rec typeOf (env : Map<string, LispVal>) = function
        | ArgRef _ -> typeof<int>
        | Atom a -> a |> ident env |> typeOf env
        | Bool _ -> typeof<bool>
        | IfPrimitive (_, thenValue, elseValue) ->
            match typeOf env thenValue with
            | t when t = typeOf env elseValue -> t
            | _ -> raise <| CompilerException("expected 'then' and 'else' branches to have same type")

        | LambdaDef (_, body) -> typeOf env body
        | LambdaRef (methodBuilder, _, _) -> methodBuilder.ReturnType
        | List (Atom a :: args) -> a |> lambdaIdent args env |> typeOf env
        | List (fn :: _) -> raise <| CompilerException(sprintf "can't invoke %A" fn)
        | List [ ] -> raise <| CompilerException("can't compile empty list")
        | ListPrimitive _ -> typeof<int>
        | Number _ -> typeof<int>
        | String _ -> typeof<string>
        | VariableDef _ -> typeof<Void>
        | VariableRef local -> local.LocalType

    and returnType env code = 
        let rec tryLast = function
            | [ item ] -> Some item
            | item :: items -> tryLast items
            | [ ] -> None

        match code |> tryLast with
        | Some value -> typeOf env value
        | None -> typeof<Void>

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
                    match argTypes, parameterTypes with
                    | [ ], [ ] -> 
                        // No args and no parameters -> always OK
                        true

                    | [ ], [ _ ] -> 
                        // No args and one parameter -> OK only for params array methods
                        isParamArray

                    | [ ], _ ->
                        // No args and two or more parameters -> never OK
                        false

                    | argType :: otherArgTypes, [ parameterType ] when isParamArray -> 
                        // One or more args and one parameter, in a params array method ->
                        //  OK if the types of the first arg and the params array are compatible,
                        //  and the rest of the args match the params array
                        parameterType.GetElementType().IsAssignableFrom(argType) 
                        && argsMatchParameters' otherArgTypes parameterTypes

                    | argType :: otherArgTypes, parameterType :: otherParameterTypes -> 
                        // One or more args and one or more parameters -> 
                        //  OK if the types of the first arg and parameter are compatible, 
                        //  and the rest of the args match the rest of the parameters
                        parameterType.IsAssignableFrom(argType) 
                        && argsMatchParameters' otherArgTypes otherParameterTypes

                    | _ :: _, [ ] -> 
                        // One or more args and no parameters -> never OK
                        false

                argsMatchParameters' (List.map (typeOf env) args) parameterTypes

            | _ -> false

        let candidates = List.append envMatches clrMatches
        match candidates with
        | [ ] -> raise <| CompilerException(sprintf "no method called %s" a)
        | _ -> ()

        let allMatches = List.filter argsMatchParameters candidates
        match allMatches with
        | [ ] -> raise <| CompilerException(sprintf "no overload of %s is compatible with %A" a args)
        | firstMatch :: _ -> firstMatch

    let rec foldBlocks func state =
        function
        | value :: otherValues ->
            let valueState, valueHead, (valueTail : ILBlock) = func state value
            let otherValuesState, otherValuesHead, otherValuesTail = foldBlocks func valueState otherValues
            valueTail |> br otherValuesHead
            otherValuesState, valueHead, otherValuesTail
        | [ ] ->
            let block = empty ()
            state, block, block

    let rec makeBlock (target : IILTarget) env =
        function
        | ArgRef index -> 
            let block = emit [ Ldarg index ]
            env, block, block

        | Atom a -> 
            a |> ident env |> makeBlock target env

        | Bool false ->
            let block = emit [ Ldc_I4_0 ]
            env, block, block

        | Bool true ->
            let block = emit [ Ldc_I4_1 ]
            env, block, block
            
        | IfPrimitive (ListPrimitive (Equal, [ a; b ]), thenValue, elseValue) ->
            let aEnv, aHead, aTail = makeBlock target env a
            let bEnv, bHead, bTail = makeBlock target aEnv b
            let _, thenHead, thenTail = makeBlock target bEnv thenValue
            let _, elseHead, elseTail = makeBlock target bEnv elseValue
            let endBlock = empty ()
            aTail |> br bHead
            bTail |> beq thenHead elseHead
            thenTail |> br endBlock
            elseTail |> br endBlock
            bEnv, aHead, endBlock

        | IfPrimitive (testValue, thenValue, elseValue) ->
            let testEnv, testHead, testTail = makeBlock target env testValue
            let _, thenHead, thenTail = makeBlock target testEnv thenValue
            let _, elseHead, elseTail = makeBlock target testEnv elseValue
            let endBlock = empty ()
            testTail |> brtrue thenHead elseHead
            thenTail |> br endBlock
            elseTail |> br endBlock
            testEnv, testHead, endBlock

        | LambdaDef _ -> 
            raise <| new NotImplementedException("didn't expect lambda outside variable")

        | LambdaRef _ -> 
            raise <| CompilerException("can't compile lambda - try invoking it instead")

        | List (Atom a :: args) ->
            match lambdaIdent args env a with
            | LambdaRef (methodInfo, isParamArray, parameterTypes) -> 
                let emitBoxed (expectedType : #Type) env x =
                    let valueEnv, valueHead, valueTail = makeBlock target env x
                    match typeOf env x with
                    | a when not expectedType.IsValueType && a.IsValueType -> 
                        let boxBlock = emit [ Box a ]
                        valueTail |> br boxBlock
                        valueEnv, valueHead, boxBlock

                    | _ ->
                        valueEnv, valueHead, valueTail

                let rec emitArgs (parameterTypes : #Type list) env args =
                    match args, parameterTypes with
                    | arg :: otherArgs, [ parameterType ] when isParamArray ->
                        let elementType = parameterType.GetElementType()

                        let rec emitArrayInit env position =
                            function
                            | value :: values ->
                                let boxedEnv, boxedHead, boxedTail = emitBoxed elementType env value
                                let block = emit [ Dup; Ldc_I4 position ]
                                let stelemBlock = emit [ Stelem elementType ]

                                block |> br boxedHead
                                boxedTail |> br stelemBlock

                                let valuesEnv, valuesHead, valuesTail = emitArrayInit boxedEnv (position + 1) values
                                stelemBlock |> br valuesHead
                                valuesEnv, block, valuesTail

                            | [ ] -> 
                                let block = empty ()
                                env, block, block

                        let initEnv, initHead, initTail = emitArrayInit env 0 args
                        let newarrBlock = emit [ Ldc_I4 <| List.length args; Newarr elementType ]
                        newarrBlock |> br initHead
                        initEnv, newarrBlock, initTail

                    | arg :: otherArgs, parameterType :: otherParameterTypes ->
                        let boxedEnv, boxedHead, boxedTail = emitBoxed parameterType env arg
                        let otherArgsEnv, otherArgsHead, otherArgsTail = emitArgs otherParameterTypes boxedEnv otherArgs
                        boxedTail |> br otherArgsHead
                        otherArgsEnv, boxedHead, otherArgsTail

                    | [ ], [ ] -> 
                        let block = empty ()
                        env, block, block

                    | _ :: _, [ ] -> 
                        raise <| new InvalidOperationException(sprintf "got %d too many args" <| List.length args)

                    | [ ], _ :: _ -> 
                        raise <| new InvalidOperationException(sprintf "got %d too few args" <| List.length parameterTypes)

                let argsEnv, argsHead, argsTail = emitArgs parameterTypes env args
                let callBlock = emit [ Call methodInfo ]
                argsTail |> br callBlock
                argsEnv, argsHead, callBlock

            | v -> raise <| new NotImplementedException(sprintf "can't invoke variable %A" v)

        | List (fn :: args) -> 
            raise <| new NotImplementedException(sprintf "can't invoke value %A" fn)

        | List [ ] -> 
            raise <| CompilerException("can't invoke empty list")

        | ListPrimitive (op, args) -> 
            match args with
            | arg :: otherArgs ->
                let opCode = 
                    match op with
                    | ListOp.Add -> Add
                    | Subtract -> Sub
                    | Multiply -> Mul
                    | Divide -> Div
                    | Equal -> Ceq

                let coerceToInt env x = 
                    let valueEnv, valueHead, valueTail = makeBlock target env x
                    match typeOf env x with
                    | t when t = typeof<obj> -> 
                        let coerceBlock = emit [ Call <| typeof<Convert>.GetMethod("ToInt32", [| typeof<obj> |]) ]
                        valueTail |> br coerceBlock
                        valueEnv, valueHead, coerceBlock

                    | t when t = typeof<int> -> 
                        valueEnv, valueHead, valueTail

                    | t -> 
                        raise <| new NotImplementedException("expected int, got " + t.Name)

                let rec emitBinaryOps env args =
                    match args with
                    | arg :: otherArgs ->
                        let valueEnv, valueHead, valueTail = coerceToInt env arg
                        let otherValuesEnv, otherValuesHead, otherValuesTail = emitBinaryOps valueEnv otherArgs
                        let opCodeBlock = emit [ opCode ]

                        valueTail |> br opCodeBlock
                        opCodeBlock |> br otherValuesHead
                        otherValuesEnv, valueHead, otherValuesTail

                    | [ ] ->
                        let block = empty ()
                        env, block, block

                let valueEnv, valueHead, valueTail = coerceToInt env arg
                let otherValuesEnv, otherValuesHead, otherValuesTail = emitBinaryOps valueEnv otherArgs
                valueTail |> br otherValuesHead
                otherValuesEnv, valueHead, otherValuesTail

            | l -> 
                raise <| CompilerException(sprintf "cannot compile list %A" l)

        | Number n -> 
            let block = emit [ Ldc_I4 n ]
            env, block, block

        | String s -> 
            let block = emit [ Ldstr s] 
            env, block, block

        | VariableDef (name, value) ->
            match value with
            | LambdaDef (paramNames, body) ->
                (*
                 * typeof<int> is a hack to get recursive functions to compile.
                 *  At this point in the compilation we don't have an env that contains 
                 *  lambda, so if the lambda tail calls, we can't determine its return type.
                 *
                 * Possible fixes:
                 *  - Change LambdaRef and Call so that they no longer use a MethodInfo. 
                 *      Then generate the .NET method for the lambda after 
                 *      its ILBlock has been constructed.
                 *  - Type inference: the types of all values possibly returned by the lambda
                 *      must match. There will be some base case where a recursive lambda returns
                 *      a known type (e.g. factorial eventually returns 1)
                 *)
                let lambdaTarget =
                    target.DefineMethod
                        name
                        typeof<int>
                        (List.replicate (List.length paramNames) (typeof<int>))

                let lambdaInfo = lambdaTarget.MethodInfo

                let envWithLambda = 
                    env 
                    |> (LambdaRef (lambdaInfo, false, (List.map (fun _ -> typeof<int>) paramNames)) 
                        |> Map.add name)

                let (envWithLambdaArgs, _) = 
                    paramNames 
                    |> ((envWithLambda, 0) 
                        |> List.fold (fun (env, index) name -> (Map.add name (ArgRef index) env, index + 1)))

                let _, bodyHead, bodyTail = makeBlock lambdaTarget envWithLambdaArgs body
                bodyTail |> ret
                lambdaTarget.GenerateIL bodyHead

                let block = empty ()
                envWithLambda, block, block

            | _ ->
                let local = target.DeclareLocal(typeOf env value)
                let envWithVariable = Map.add name (VariableRef local) env
                let _, variableHead, variableTail = makeBlock target envWithVariable value

                let block = emit [ Stloc local ]
                variableHead |> br block

                envWithVariable, variableHead, block

        | VariableRef local -> 
            let block = emit [ Ldloc local ]
            env, block, block

    let compile target env = 
        let func env = insertPrimitives >> makeBlock target env
        foldBlocks func env
