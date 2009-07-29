#light
namespace Tim.Lisp.Core
open System.Reflection
open System.Reflection.Emit

type MethodBuilderTarget(typeBuilder : TypeBuilder, methodBuilder : MethodBuilder) = 
    inherit ILTargetBase(methodBuilder.GetILGenerator(), methodBuilder)
    override this.DefineMethodCore name returnType parameterTypes = 
        let target = 
            new MethodBuilderTarget(
                typeBuilder,
                typeBuilder.DefineMethod(name, MethodAttributes.Static ||| MethodAttributes.Private, returnType, Array.of_list parameterTypes))
        target :> IILTarget
