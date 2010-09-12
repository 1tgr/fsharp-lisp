#light
namespace Tim.Lisp.Compiler

open System
open System.CodeDom.Compiler
open System.IO
open Tim.Lisp.Core

module Program =
    let glob (wildcard : string) : string array =
        Directory.GetFiles(
            (match Path.GetDirectoryName(wildcard) with | "" -> "." | dir -> dir),
            Path.GetFileName(wildcard))

    [<EntryPoint>]
    let main (filenames : string array) =
        try
            let provider = new LispCodeProvider()
            let options = new CompilerParameters()
            options.OutputAssembly <- "output.exe"

            let results = provider.CompileAssemblyFromFile(options, Array.collect glob filenames)
            let errors = results.Errors |> Seq.cast<CompilerError>
            errors |> Seq.iter (fun e -> stderr.WriteLine(e.ErrorText))

            if errors |> Seq.exists (fun e -> not e.IsWarning)
            then 1
            else 0
        with ex ->
            stderr.WriteLine(ex)
            1