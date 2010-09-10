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
    let builtins =
        @"
(.using System)"
        |> Parser.parseString

    let delegateType (result : obj option) =
        match result with
        | None -> typeof<Action>
        | Some value -> typedefof<Func<_>>.MakeGenericType(value.GetType())

module CompilerTests =
    let programs = [ "6",                                               Some <| box 6
                     "\"hello\"",                                       Some <| box "hello"
                     @"(define number 6)
                       number",                                         Some <| box 6
                     "(.asm ldc.i4.0 Int32)",                           Some <| box 0
                     "(.asm (ldc.i4 6) Int32)",                         Some <| box 6
                     "(.asm add Int32 2 4)",                            Some <| box 6
                     @"(define (char-upcase c) (.asm (call Char.ToUpperInvariant Char) Char c))
                       (char-upcase #\x)",                              Some <| box 'X'
                     @"(.asm add Int32
                         (.asm (ldc.i4 2) Int32) 
                         (.asm (ldc.i4 4) Int32))",                     Some <| box 6
                     @"(define (factorial n)
                         (if (= n 0) 
                           1 
                           (* n (factorial (- n 1)))))
                       (factorial 6)",                                  Some <| box 720
                     @"(define (factorial n acc)
                         (if (= n 0)
                           acc
                           (factorial (- n 1) (* acc n))))
                       (factorial 6 1)",                                Some <| box 720
                     @"(define (countTo total acc)
                         (if (= total acc)
                           acc
                           (countTo total (+ 1 acc))))
                       (countTo 10000000 0)",                           Some <| box 10000000 ]

    let data = List.map (fun (a, b) -> [| box a; box b |]) programs

    [<Theory; PropertyData("data")>]
    let parses (source : string, _ : obj option) =
        source
        |> Parser.parseString
        |> ignore

    [<Theory; PropertyData("data")>]
    let compiles (source : string, result : obj option) =
        Utils.builtins @ Parser.parseString source
        |> Compiler.compileToDelegate (Utils.delegateType result)
        |> ignore
          
    [<Theory; PropertyData("data")>]
    let verifies (source : string, _ : obj option) =
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
                Utils.builtins @ Parser.parseString (sprintf "(define (test _) %s) 0" source)
                |> Compiler.compileToFile "DynamicAssembly.exe"

                use proc = new Process()

                proc.StartInfo <-
                    let peverify = Path.Combine(string folder, "peverify.exe")
                    let si = new ProcessStartInfo(peverify, "/nologo DynamicAssembly.exe")
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

                Assert.Equal("All Classes and Methods in DynamicAssembly.exe Verified.", string sb)

    [<Theory; PropertyData("data")>]
    let runs (source : string, result : obj option) =
        let d =
            Utils.builtins @ Parser.parseString source
            |> Compiler.compileToDelegate (Utils.delegateType result)

        match d with
        | :? Action as a ->
            a.Invoke()

        | _ ->
            let actualResult = d.DynamicInvoke()
            Assert.Equal(Option.get result, actualResult)
