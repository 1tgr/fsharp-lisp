#light
namespace Tim.Lisp.Core.UnitTests
open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Text
open Microsoft.Win32
open Xunit
open Xunit.Extensions
open Tim.Lisp.Core

module Utils =
    type Dummy = Dummy

    let assembly = typeof<Dummy>.Assembly
    let sampleNames = 
        assembly.GetManifestResourceNames()
        |> Array.filter (fun s -> s.EndsWith(".scm"))

    let builtins =
        @"
(.ref ""xunit.dll"")
(.using System)
(.using Xunit)
(define (assert-equal expected actual)
(.asm (call Assert.Equal Int32 Int32) Void expected actual))"
        |> Parser.parseString

    let load (name : string) : string =
        use stream = assembly.GetManifestResourceStream(name)
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let parse (name : string) : Syntax.Expr list =
        let source = load name
        builtins @ Parser.parseString source

module CompilerTests =
    let data = Array.map (fun a -> [| box a |]) Utils.sampleNames
        
    [<Theory; PropertyData("data")>]
    let parses (name : string) =
        name
        |> Utils.parse
        |> ignore

    [<Theory; PropertyData("data")>]
    let compiles (name : string) =
        name
        |> Utils.parse
        |> Compiler.compileToDelegate typeof<Action>
        |> ignore
          
    [<Theory; PropertyData("data")>]
    let verifies (name : string) =
        let version = 
            match RuntimeEnvironment.GetSystemVersion() with
            | "v2.0.50727" -> new Version(3, 5)
            | s -> new Version(s.Substring(1))

        let path = sprintf @"SOFTWARE\Microsoft\Microsoft SDKs\Windows\v7.0A\WinSDK-NetFx%d%dTools" version.Major version.Minor

        using (Registry.LocalMachine.OpenSubKey(path)) <|
        function
        | null -> ()
        | key ->
            match key.GetValue("InstallationFolder") with
            | null -> ()
            | folder ->
                let filename = Path.ChangeExtension(name, ".exe")

                name
                |> Utils.parse
                |> Compiler.compileToFile filename

                use proc = new Process()

                proc.StartInfo <-
                    let peverify = Path.Combine(string folder, "peverify.exe")
                    let si = new ProcessStartInfo(peverify, sprintf "/nologo %s" filename)
                    si.CreateNoWindow <- true
                    si.UseShellExecute <- false
                    si.RedirectStandardError <- true
                    si.RedirectStandardOutput <- true
                    si

                let sb = new StringBuilder()
                let syncRoot = new obj()

                let outputDataReceived (e : DataReceivedEventArgs) =
                    if not (String.IsNullOrEmpty(e.Data)) then
                        lock syncRoot <| fun _ ->
                            if sb.Length > 0 then
                                ignore <| sb.Append(Environment.NewLine)

                            ignore <| sb.Append(e.Data)

                using (proc.OutputDataReceived.Subscribe outputDataReceived) <|
                fun _ ->
                    ignore <| proc.Start()
                    proc.BeginErrorReadLine()
                    proc.BeginOutputReadLine()
                    proc.WaitForExit()

                Assert.Equal(sprintf "All Classes and Methods in %s Verified." filename, string sb)

    [<Theory; PropertyData("data")>]
    let runs (name : string) =
        let d =
            name
            |> Utils.parse
            |> Compiler.compileToDelegate typeof<Action>

        (d :?> Action).Invoke()
