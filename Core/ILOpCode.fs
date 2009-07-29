#light
namespace Tim.Lisp.Core
open System
open System.Reflection
open System.Reflection.Emit

type ILOpCode = 
              | Add
              | Box of Type
              | Call of MethodInfo
              | Ceq
              | Div
              | Dup
              | Ldarg of int
              | Ldc_I4 of int
              | Ldc_I4_0
              | Ldc_I4_1
              | Ldloc of LocalBuilder
              | Ldstr of string
              | Mul
              | Newarr of Type
              | Pop
              | Stelem of Type
              | Stloc of LocalBuilder
              | Sub
