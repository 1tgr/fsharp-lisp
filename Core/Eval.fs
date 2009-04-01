#light
namespace Tim.Lisp.Core

[<Sealed>]
type Eval =
    static member Eval(v) = Evaluator.eval Map.empty v |> snd