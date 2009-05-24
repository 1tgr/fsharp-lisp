#light
namespace Tim.Lisp.Core

type MaybeBuilder() =
    member b.Return(x) = Some x
    member b.Bind(p, rest) = 
        match p with
        | None -> None
        | Some r -> rest r
    member b.Delay(f) = f()
    member b.Let(p, rest) : Option<'a> = rest p

module MaybeBuilderModule =
    let maybe = MaybeBuilder()
    let option_of_nullable o =
        match box o with
        | null -> None
        | _ -> Some o
