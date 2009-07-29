#light
namespace Tim.Lisp.Core
open System.Reflection
open System.Reflection.Emit

type DynamicMethodTarget(dynamicMethod : DynamicMethod) = 
    inherit ILTargetBase(dynamicMethod.GetILGenerator(), dynamicMethod)
    override this.DefineMethodCore name returnType parameterTypes = 
        let target = new DynamicMethodTarget(new DynamicMethod(name, returnType, Array.of_list parameterTypes))
        target :> IILTarget
