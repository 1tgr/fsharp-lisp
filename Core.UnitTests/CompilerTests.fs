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
(.ref ""xunit.dll"")
(.using System)
(.using Xunit)
(define (assert-equal expected actual)
        (.asm (call Assert.Equal Int32 Int32) Void expected actual))"
        |> Parser.parseString

module CompilerTests =
    let programs = [ "(assert-equal 6 6)"
                     @"(.asm (call Assert.Equal String String) Void ""hello"" ""hello"")"
                     @"(define number 6)
                       (assert-equal 6 number)"
                     "(assert-equal 0 (.asm ldc.i4.0 Int32))"
                     "(assert-equal 6 (.asm (ldc.i4 6) Int32))"
                     "(assert-equal 6 (.asm add Int32 2 4))"
                     //@"(define (char-upcase c) (.asm (call Char.ToUpperInvariant Char) Char c))
                     //  (assert-equal #\X (char-upcase #\x))"
                     @"(assert-equal
                         6
                            (.asm add Int32
                            (.asm (ldc.i4 2) Int32) 
                            (.asm (ldc.i4 4) Int32)))"
                     @"(define (factorial n)
                         (if (= n 0) 
                           1 
                           (* n (factorial (- n 1)))))
                       (assert-equal 720 (factorial 6))"
                     @"(define (factorial n acc)
                         (if (= n 0)
                           acc
                           (factorial (- n 1) (* acc n))))
                       (assert-equal 720 (factorial 6 1))"
                     @"(define (countTo total acc)
                         (if (= total acc)
                           acc
                           (countTo total (+ 1 acc))))
                       (assert-equal 10000000 (countTo 10000000 0))" ]

    let data = List.map (fun a -> [| box a |]) programs

    [<Theory; PropertyData("data")>]
    let parses (source : string) =
        source
        |> Parser.parseString
        |> ignore

    [<Theory; PropertyData("data")>]
    let compiles (source : string) =
        Utils.builtins @ Parser.parseString source
        |> Compiler.compileToDelegate typeof<Action>
        |> ignore
          
    [<Theory; PropertyData("data")>]
    let verifies (source : string) =
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
    let runs (source : string) =
        let d =
            Utils.builtins @ Parser.parseString source
            |> Compiler.compileToDelegate typeof<Action>

        (d :?> Action).Invoke()
