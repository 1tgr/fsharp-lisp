#light
namespace Tim.Lisp.Core
open System
open System.CodeDom.Compiler
open System.Reflection

type LispCodeProvider() =
    inherit CodeDomProvider()

    override this.CreateCompiler() =
        { new ICodeCompiler with
            member this.CompileAssemblyFromDom(options, compileUnit) = this.CompileAssemblyFromDomBatch(options, [| compileUnit |])
            member this.CompileAssemblyFromDomBatch(options, compileUnits) = raise <| new NotSupportedException()
            member this.CompileAssemblyFromFile(options, filename) = this.CompileAssemblyFromFileBatch(options, [| filename |])
            member this.CompileAssemblyFromFileBatch(options, filenames) = raise <| new NotSupportedException()
            member this.CompileAssemblyFromSource(options, source) = this.CompileAssemblyFromSourceBatch(options, [| source |])
            member this.CompileAssemblyFromSourceBatch(options, sources) = 
                let statements = 
                    sources
                    |> List.ofArray
                    |> List.collect Parser.parseString

                let results = new CompilerResults(new TempFileCollection())
                
                try
                    if options.GenerateInMemory
                    then
                        let (assembly, _, _) = Compiler.compileToMemory "output" statements
                        results.CompiledAssembly <- assembly
                    else
                        Compiler.compileToFile options.OutputAssembly statements
                with ex ->
                    new CompilerError("<source>", 1, 1, "0100", ex.Message) 
                    |> results.Errors.Add 
                    |> ignore

                results
        }

    override this.CreateGenerator() = raise <| new NotSupportedException()
