﻿// Copyright (c) Stephan Tolksdorf 2009
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.StateTests

open FParsec
open FParsec.Test.Test

let EOS = CharStream.Iterator.EndOfStreamChar

// creates a test CharStream with blocksize 8 (if the content is large enough)
let createTestStreamCharStream (content: char[]) =
    let e = System.Text.Encoding.Unicode
    let bs = e.GetBytes(content)
    new CharStream(new System.IO.MemoryStream(bs, false), false, System.Text.Encoding.Unicode, false,
                                                                                                     #if LOW_TRUST
                                                                                                     #else
                                                                                                         8, 3, 3,
                                                                                                     #endif
                                                                                                         16)

let testEmptyStream() =
    use stream = new CharStream("")
    let s0 = new State<_>(stream, 'a')
    s0.Next |> Equal s0

    s0.Advance(0) |> Equal s0
    s0.Advance(1) |> Equal s0
    s0.Advance(System.Int32.MaxValue) |> Equal s0
    try s0.Advance(-1) |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()
    try s0.Advance(System.Int32.MinValue) |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()

    let sb = new State<_>(stream, 'b')
    s0.Advance(0, 'b') |> Equal sb
    s0.Advance(1, 'b') |> Equal sb
    s0.Advance(System.Int32.MaxValue, 'b') |> Equal sb
    try s0.Advance(-1, 'b') |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()
    try s0.Advance(System.Int32.MinValue, 'b') |> ignore; Fail()
    with :? System.ArgumentOutOfRangeException -> ()

    s0.SkipWhitespace() |> ReferenceEqual s0
    s0.SkipNewline()|> ReferenceEqual s0
    s0.SkipRestOfLine(false) |> ReferenceEqual s0
    s0.SkipRestOfLine(true) |> ReferenceEqual s0
    let mutable str = null
    s0.SkipRestOfLine(false, &str) |> ReferenceEqual s0
    s0.SkipRestOfLine(true, &str) |> ReferenceEqual s0
    s0.SkipCharOrNewline()|> ReferenceEqual s0
    s0.SkipCharsOrNewlines(1, &str)|> ReferenceEqual s0
    s0.SkipCharsOrNewlines(System.Int32.MaxValue, &str)|> ReferenceEqual s0
    let mutable n = 0
    s0.SkipCharsOrNewlines(1, &n)|> ReferenceEqual s0
    s0.SkipCharsOrNewlines(System.Int32.MaxValue, &n)|> ReferenceEqual s0
    s0.SkipCharsOrNewlinesWhile((fun c -> true)) |> ReferenceEqual s0
    s0.SkipCharsOrNewlinesWhile((fun c -> true), &str) |> ReferenceEqual s0
    s0.SkipCharsOrNewlinesWhile((fun c -> true), 0, 1) |> ReferenceEqual s0
    s0.SkipCharsOrNewlinesWhile((fun c -> true), 0, System.Int32.MaxValue) |> ReferenceEqual s0
    s0.SkipCharsOrNewlinesWhile((fun c -> true), 0, 1, &str) |> ReferenceEqual s0
    s0.SkipCharsOrNewlinesWhile((fun c -> true), 0, System.Int32.MaxValue, &str) |> ReferenceEqual s0
    s0.SkipToString("1", 1, &str) |> ReferenceEqual s0
    s0.SkipToString("1", System.Int32.MaxValue, &str) |> ReferenceEqual s0
    s0.SkipToStringCI("1", 1, &str) |> ReferenceEqual s0
    s0.SkipToStringCI("1", System.Int32.MaxValue, &str) |> ReferenceEqual s0
    let mutable b = false
    s0.SkipToString("1", 1, &b) |> ReferenceEqual s0
    s0.SkipToString("1", System.Int32.MaxValue, &b) |> ReferenceEqual s0
    s0.SkipToStringCI("1", 1, &b) |> ReferenceEqual s0
    s0.SkipToStringCI("1", System.Int32.MaxValue, &b) |> ReferenceEqual s0

let testAdvance() =
    let refString = "1234567890ABCDEF"
    let N = refString.Length
    use stream = createTestStreamCharStream (refString.ToCharArray())
    let s0 = new State<_>(stream, 0)

    let test i j =
        let c0 = refString.[0]
        let ii = int64 (min i N)
        let ci = if i < N then refString.[i] else EOS
        let jj = int64 (min j N)
        let cj = if j < N then refString.[j] else EOS
        let d = j - min i N

        s0.Index |> Equal 0L
        s0.Iter.Read() |> Equal c0
        let si = s0.Advance(i)
        si.Index |> Equal ii
        si.Iter.Read() |> Equal ci

        let sj = si.Advance(d)
        sj.Index |> Equal jj
        sj.Iter.Read() |> Equal cj
        if d = 1 then
            si.Next |> Equal sj
        let sj_2 = si.Advance(d, -1)
        sj_2.Index |> Equal jj
        sj_2.UserState |> Equal -1
        sj_2.Iter.Read() |> Equal cj
        let sj_3 = si.Advance(d, 1, 0)
        sj_3.Index |> Equal jj
        sj_3.Iter.Read() |> Equal cj
        sj_3.Line   |> Equal 2L
        sj_3.Column |> Equal 1L
        let sj_4 = si.Advance(d, 1, 0, -1)
        sj_4.Index |> Equal jj
        sj_4.Iter.Read() |> Equal cj
        sj_4.Line   |> Equal 2L
        sj_4.Column |> Equal 1L
        sj_2.UserState |> Equal -1

    for i = 0 to N + 2 do
        for j = 0 to N + 2 do
            test i j

    use stream = new CharStream("\n\n\n", 1, 2, 100L)
    let s1 = new State<_>(stream, ())
    s1.Index     |> Equal 100L
    let s2 = s1.Advance(1, 1, 0)
    s2.Index     |> Equal 101L
    s2.LineBegin |> Equal 101L
    let s2 = s1.Advance(1, 1, 0, ())
    s2.Index     |> Equal 101L
    s2.LineBegin |> Equal 101L

    let testException() =
        let sn = s0.Advance(N)
        let sn_1 = s0.Advance(N, -1)
        s0.Advance(System.Int32.MaxValue) |> Equal sn
        s0.Advance(System.Int32.MaxValue, -1) |> Equal sn_1
        sn.Advance(System.Int32.MaxValue) |> Equal sn
        sn.Advance(System.Int32.MaxValue, -1) |> Equal sn_1
        for i = 0 to N do
            let si = s0.Advance(i)
            try si.Advance(-i - 1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try si.Advance(-i - 1, -1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try si.Advance(System.Int32.MinValue) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()
            try si.Advance(System.Int32.MinValue, -1) |> ignore; Fail()
            with :? System.ArgumentOutOfRangeException -> ()

    testException()

    let testAdvanceTo() =
        let s1    = s0.Advance(1)
        let s11   = s0.Advance(1, -1)
        let s110  = s0.Advance(1, 1, 0)
        let s1101 = s0.Advance(1, 1, 0, -1)
        let iter1 = s0.Iter.Next
        s0.AdvanceTo(iter1) |> Equal s1
        s0.AdvanceTo(iter1, -1) |> Equal s11
        s0.AdvanceTo(iter1, 1, 0) |> Equal s110
        s0.AdvanceTo(iter1, 1, 0, -1) |> Equal s1101
        s0.AdvanceTo(iter1, 1u, 0u) |> Equal s110
        s0.AdvanceTo(iter1, 1u, 0u, -1) |> Equal s1101
        s0.AdvanceTo(iter1, 1L, 0L) |> Equal s110
        s0.AdvanceTo(iter1, 1L, 0L, -1) |> Equal s1101

    testAdvanceTo()

let testSkipWhitespace() =
    // check fast path

    let testChars = [|'\t'; '\n'; '\r'; ' '; '\u0008'; '\u000C'; '\u0021'; |]

    let checkSkipWhitespace (cs: char[]) iBegin (sBegin: State<unit>) =
        let mutable line = 1
        let mutable lineBegin = 0
        let mutable i = iBegin
        // another independent skipWhitespace implementation...
        while i < cs.Length
              && (match cs.[i] with
                  | ' ' | '\t' -> true
                  | '\r' ->
                      line <- line + 1; lineBegin <- i + 1; true
                  | '\n' ->
                      if i = iBegin || cs.[i - 1] <> '\r' then
                          line <- line + 1
                      lineBegin <- i + 1
                      true
                  | _ -> false)
           do i <- i + 1
        let sEnd = sBegin.SkipWhitespace()
        if i = iBegin then sBegin |> ReferenceEqual sEnd
        else
            let indexOffset = int32 sBegin.Index - iBegin
            int32 sEnd.Index |> Equal (i + indexOffset)
            int32 sEnd.Line  |> Equal line
            int32 sEnd.LineBegin |> Equal (if line <> 1 then lineBegin + indexOffset else int32 sBegin.LineBegin)
            sEnd.Iter.Read() |> Equal (if i < cs.Length then cs.[i] else EOS)

    let testFastPath() =
        let cs = Array.create 11 '_'
    #if LOW_TRUST
    #else
        use stream = new CharStream(cs, 1, 10, 100L)
        let s1 = new State<unit>(stream, ())
    #endif
        for c1 in testChars do
            cs.[1] <- c1
            for c2 in testChars do
                cs.[2] <- c2
                for c3 in testChars do
                    cs.[3] <- c3
                    for c4 in testChars do
                        cs.[4] <- c4
                        for c5 in testChars do
                            cs.[5] <- c5
                            for c6 in testChars do
                                cs.[6] <- c6
                                for c7 in testChars do
                                    cs.[7] <- c7
                                #if LOW_TRUST
                                    let stream = new CharStream(new string(cs), 1, 10, 100L)
                                    let s1 = new State<unit>(stream, ())
                                #endif
                                    checkSkipWhitespace cs 1 s1

        // check end of block/stream handling
    #if LOW_TRUST
    #else
        let s7  = s1.Advance(6)
        let s8  = s1.Advance(7)
        let s9  = s1.Advance(8)
        let s10 = s1.Advance(9)
    #endif
        for c7 in testChars do
            cs.[7] <- c7
            for c8 in testChars do
                cs.[8] <- c8
                for c9 in testChars do
                    cs.[9] <- c9
                    for c10 in testChars do
                        cs.[10] <- c10
                    #if LOW_TRUST
                        let stream = new CharStream(new string(cs), 1, 10, 100L)
                        let s1 = new State<unit>(stream, ())
                        let s7  = s1.Advance(6)
                        let s8  = s1.Advance(7)
                        let s9  = s1.Advance(8)
                        let s10 = s1.Advance(9)
                    #endif
                        checkSkipWhitespace cs 7  s7
                        checkSkipWhitespace cs 8  s8
                        checkSkipWhitespace cs 9  s9
                        checkSkipWhitespace cs 10 s10

    #if LOW_TRUST
        let stream = new CharStream(new string(cs), 1, 10, 100L)
        let s1 = new State<unit>(stream, ())
    #endif
        let s11 = s1.Advance(10)
        s11.SkipWhitespace() |> ReferenceEqual s11

    let testSlowPath() =
        let cs =  Array.create 17 '_'
        // check end of block handling with multi-block CharStream (blockSize = 8, blockOverlap = 3)
        for c4 in testChars do
            cs.[4] <- c4
            for c5 in testChars do
                cs.[5] <- c5
                for c6 in testChars do
                    cs.[6] <- c6
                    for c7 in testChars do
                        cs.[7] <- c7
                        for c8 in testChars do
                            cs.[8] <- c8
                            use stream = createTestStreamCharStream cs
                            let s0 = (new State<_>(stream, ()))
                            let s4 = s0.Advance(4)
                            checkSkipWhitespace cs 4 s4 // will start in the fast path
                            s0.Iter.Read() |> ignore // make sure the first block is read in
                            let s5 = s0.Advance(5)
                            checkSkipWhitespace cs 5 s5 // will start in the slow path

    testFastPath()
    testSlowPath()

let testSkipRestOfLine() =
    let testChars = [|'\n'; '\r'; '\t'; '\u000C'; '\u000E'; |]

    let checkSkipRestOfLine (cs: char[]) iBegin (sBegin: State<unit>) =
        let stream = sBegin.Stream
        let iter0 = stream.Begin
        let indexOffset = int32 sBegin.Index - iBegin
        let mutable i = iBegin
        while i < cs.Length  && (cs.[i] <> '\r' && cs.[i] <> '\n') do i <- i + 1
        iter0.Read() |> ignore; sBegin.Iter.Read() |> ignore
        let sEnd1 = sBegin.SkipRestOfLine(false)
        iter0.Read() |> ignore; sBegin.Iter.Read() |> ignore
        let mutable str = null
        let sEnd2 = sBegin.SkipRestOfLine(false, &str)
        sEnd1 |> Equal sEnd2
        if i = iBegin then
            sBegin |> ReferenceEqual sEnd1
            sBegin |> ReferenceEqual sEnd2
            str.Length |> Equal 0
        else
            str.Length |> Equal (i - iBegin)
            sBegin.Iter.Match(str) |> True
            int32 sEnd1.Index |> Equal (i + indexOffset)
            int32 sEnd1.Line  |> Equal 1
            sEnd1.LineBegin   |> Equal sBegin.LineBegin
            sEnd1.Iter.Read() |> Equal (if i < cs.Length then cs.[i] else EOS)

        let mutable line = 1
        if i < cs.Length then
            let c = cs.[i]
            if c = '\r' || c = '\n' then
                i <- i + if c = '\r' && i + 1 < cs.Length && cs.[i + 1] = '\n' then 2 else 1
                line <- 2

        iter0.Read() |> ignore; sBegin.Iter.Read() |> ignore
        let sEnd3 = sBegin.SkipRestOfLine(true)
        let mutable str2 = null
        iter0.Read() |> ignore; sBegin.Iter.Read() |> ignore
        let sEnd4 = sBegin.SkipRestOfLine(true, &str2)
        sEnd3 |> Equal sEnd4
        str2 |> Equal str
        if i = iBegin then
            sBegin |> ReferenceEqual sEnd3
            sBegin |> ReferenceEqual sEnd4
        else
            int32 sEnd3.Index |> Equal (i + indexOffset)
            int32 sEnd3.Line  |> Equal line
            int32 sEnd3.LineBegin |> Equal (if line <> 1 then i + indexOffset else int32 sBegin.LineBegin)
            sEnd3.Iter.Read() |> Equal (if i < cs.Length then cs.[i] else EOS)

    let testFastPath() =
        let cs = Array.create 8 '_'
    #if LOW_TRUST
    #else
        use stream = new CharStream(cs, 1, 7, 100L)
        let s1 = new State<unit>(stream, ())
    #endif
        for c1 in testChars do
            cs.[1] <- c1
            for c2 in testChars do
                cs.[2] <- c2
                for c3 in testChars do
                    cs.[3] <- c3
                    for c4 in testChars do
                        cs.[4] <- c4
                        for c5 in testChars do
                            cs.[5] <- c5
                        #if LOW_TRUST
                            use stream = new CharStream(new string(cs), 1, 7, 100L)
                            let s1 = new State<unit>(stream, ())
                        #endif
                            checkSkipRestOfLine cs 1 s1


        // check end of block/stream handling
    #if LOW_TRUST
    #else
        let s5 = s1.Advance(4)
        let s6 = s1.Advance(5)
        let s7 = s1.Advance(6)
    #endif
        for c5 in testChars do
            cs.[5] <- c5
            for c6 in testChars do
                cs.[6] <- c6
                for c7 in testChars do
                    cs.[7] <- c7
                #if LOW_TRUST
                    use stream = new CharStream(new string(cs), 1, 7, 100L)
                    let s1 = new State<unit>(stream, ())
                    let s5 = s1.Advance(4)
                    let s6 = s1.Advance(5)
                    let s7 = s1.Advance(6)
                #endif
                    checkSkipRestOfLine cs 5 s5
                    checkSkipRestOfLine cs 6 s6
                    checkSkipRestOfLine cs 7 s7

    #if LOW_TRUST
        use stream = new CharStream(new string(cs), 1, 7, 100L)
        let s1 = new State<unit>(stream, ())
    #endif
        let s8 = s1.Advance(7)
        checkSkipRestOfLine cs 8 s8

    let testSlowPath() =
        let cs =  Array.create 17 '_'
        // check end of block handling with multi-block CharStream (blockSize = 8, blockOverlap = 3)
        for c5 in testChars do
            cs.[5] <- c5
            for c6 in testChars do
                cs.[6] <- c6
                for c7 in testChars do
                    cs.[7] <- c7
                    for c8 in testChars do
                        cs.[8] <- c8
                        use stream = createTestStreamCharStream cs
                        let s0 = (new State<_>(stream, ()))
                        let s5 = s0.Advance(5)
                        checkSkipRestOfLine cs 5 s5 // will start in the fast path
                        let s6 = s0.Advance(6)
                        checkSkipRestOfLine cs 6 s6 // will start in the slow path

    testFastPath()
    testSlowPath()

let testSkipCharsOrNewlines() =
    let counter = ref 0

    let check (sBegin: State<int>) (cs: char[]) iBegin nMax =
        let stream = sBegin.Stream
        let indexOffset = int32 sBegin.Index - iBegin
        let mutable iter0 = stream.Begin
        let mutable iterBegin = sBegin.Iter
        let alwaysTrue  = fun (c: char) -> true
        let alwaysFalse = fun (c: char) -> false
        let nTrueN = ref 0
        let nTrue = fun (c: char) -> if !nTrueN > 0 then decr nTrueN; true else false
        for n = 0 to nMax do
            incr counter
            let mutable line = 1
            let mutable lineBegin = 0
            let mutable i = iBegin
            let mutable c = 0
            while c < n && i < cs.Length do
                match cs.[i] with
                | '\r' | '\n' ->
                    i <- i + if cs.[i] = '\r' && i + 1 < cs.Length && cs.[i + 1] = '\n' then 2 else 1
                    line <- line + 1
                    lineBegin <- i
                | _ -> i <- i + 1
                c <- c + 1

            let c = c
            let sEndRef = if c = 0 then sBegin
                          elif line = 1 then sBegin.Advance(i - iBegin)
                          else sBegin.Advance(i - iBegin, line - 1, i - lineBegin)

            let checkOutputState (sEnd: State<int>) = // this function needs to be fast
                if c = 0 then
                    if not (System.Object.ReferenceEquals(sEnd, sEndRef)) then
                        Fail()
                else
                    if not (sEnd.Equals(sEndRef)) then
                        Fail()

            let originalStr = sBegin.Iter.Read(i - iBegin)
            let str = CharStream.NormalizeNewlines(originalStr)

            if n = 1 then
                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let sEndA = sBegin.SkipCharOrNewline()
                checkOutputState sEndA

                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let sEndB = sBegin.SkipNewline()
                if line <> 1 then checkOutputState sEndB
                else sEndB |> ReferenceEqual sBegin

                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let sEndC = sBegin.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysFalse)
                checkOutputState sEndC

                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let mutable strD = null
                let sEndD = sBegin.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysFalse, &strD)
                strD |> Equal str
                checkOutputState sEndD

                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let sEndE = sBegin.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysFalse, 0, System.Int32.MaxValue)
                checkOutputState sEndE

                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let mutable strF = null
                let sEndF = sBegin.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysFalse, 0, System.Int32.MaxValue, &strF)
                strF |> Equal str
                checkOutputState sEndF

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            let mutable nSkipped = -1
            let sEnd1 = sBegin.SkipCharsOrNewlines(n, &nSkipped)
            nSkipped |> Equal c
            checkOutputState sEnd1

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            let sEnd1_b = sBegin.SkipCharsOrNewlines(n)
            checkOutputState sEnd1_b

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            let mutable str2 = null
            let sEnd2 = sBegin.SkipCharsOrNewlines(n, &str2)
            str2 |> Equal str
            checkOutputState sEnd2

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            nTrueN:= n
            let sEnd3 = sBegin.SkipCharsOrNewlinesWhile(nTrue)
            checkOutputState sEnd3

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            nTrueN:= n
            let mutable str3 = null
            let sEnd4 = sBegin.SkipCharsOrNewlinesWhile(nTrue, &str3)
            str3 |> Equal str
            checkOutputState sEnd4

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            nTrueN:= n
            let sEnd5 = sBegin.SkipCharsOrNewlinesWhile(nTrue, 0, System.Int32.MaxValue)
            checkOutputState sEnd5

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            nTrueN:= n
            let mutable str6 = null
            let sEnd6 = sBegin.SkipCharsOrNewlinesWhile(nTrue, 0, System.Int32.MaxValue, &str6)
            str6 |> Equal str
            checkOutputState sEnd6

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            nTrueN:= n
            let sEnd7 = sBegin.SkipCharsOrNewlinesWhile(alwaysTrue, 0, n)
            checkOutputState sEnd7

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            nTrueN:= n
            let mutable str8 = null
            let sEnd8 = sBegin.SkipCharsOrNewlinesWhile(alwaysTrue, 0, n, &str8)
            str8 |> Equal str
            checkOutputState sEnd8

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            let mutable foundString = false;
            let sEnd9 = sBegin.SkipToString("\u0000", n, &foundString) // there's no '\u0000' in the input
            foundString |> False
            checkOutputState sEnd9

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            let sEnd10 = sBegin.SkipToStringCI("\u0000", n, &foundString)
            foundString |> False
            checkOutputState sEnd10

            iter0.Read() |> ignore; iterBegin.Read() |> ignore
            let mutable str11 = null : string
            let sEnd11 = sBegin.SkipToString("\u0000", n, &str11)
            str11 |> Equal null
            checkOutputState sEnd11

            iter0.Read()|> ignore; iterBegin.Read() |> ignore
            let mutable str12 = null : string
            let sEnd12 = sBegin.SkipToStringCI("\u0000", n, &str12)
            str12 |> Equal null
            checkOutputState sEnd12

            if i < cs.Length && originalStr.IndexOf(cs.[i]) = -1 then
                let ciStr = string (cs.[i])
                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let mutable foundString = false
                let sEnd13 = sBegin.SkipToString(ciStr, nMax, &foundString)
                foundString |> Equal true
                checkOutputState sEnd13

                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let sEnd14 = sBegin.SkipToStringCI(ciStr, nMax, &foundString)
                foundString |> Equal true
                checkOutputState sEnd14

                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let mutable str15 = null : string
                let sEnd15 = sBegin.SkipToString(ciStr, nMax, &str15)
                str15 |> Equal str
                checkOutputState sEnd15

                iter0.Read() |> ignore; iterBegin.Read() |> ignore
                let mutable str16 = null : string
                let sEnd16 = sBegin.SkipToStringCI(ciStr, nMax, &str16)
                str16 |> Equal str
                checkOutputState sEnd16

    let testChars = [|'\n'; '\r'; '\t'; '\u000C'; '\u000E'|]

    let testFastPath() =
        let cs = Array.create 11 '_'
    #if LOW_TRUST
    #else
        use stream = new CharStream(cs, 1, 10, 100L)
        let s1 = new State<_>(stream, -1)
    #endif
        for c1 in testChars do
            cs.[1] <- c1
            for c2 in testChars do
                cs.[2] <- c2
                for c3 in testChars do
                    cs.[3] <- c3
                    for c4 in testChars do
                        cs.[4] <- c4
                        for c5 in testChars do
                            cs.[5] <- c5
                            for c6 in testChars do
                                cs.[6] <- c6
                                for c7 in testChars do
                                    cs.[7] <- c7
                                #if LOW_TRUST
                                    use stream = new CharStream(new string(cs), 1, 10, 100L)
                                    let s1 = new State<_>(stream, -1)
                                #endif
                                    check s1 cs 1 7

        // check end of block/stream handling
    #if LOW_TRUST
    #else
        let s7  = s1.Advance(6)
        let s8  = s1.Advance(7)
        let s9  = s1.Advance(8)
        let s10 = s1.Advance(9)
    #endif
        for c7 in testChars do
            cs.[7] <- c7
            for c8 in testChars do
                cs.[8] <- c8
                for c9 in testChars do
                    cs.[9] <- c9
                    for c10 in testChars do
                        cs.[10] <- c10
                    #if LOW_TRUST
                        use stream = new CharStream(new string(cs), 1, 10, 100L)
                        let s1 = new State<_>(stream, -1)
                        let s7  = s1.Advance(6)
                        let s8  = s1.Advance(7)
                        let s9  = s1.Advance(8)
                        let s10 = s1.Advance(9)
                    #endif
                        check s7  cs  7 5
                        check s8  cs  8 4
                        check s9  cs  9 3
                        check s10 cs 10 2
    #if LOW_TRUST
        use stream = new CharStream(new string(cs), 1, 10, 100L)
        let s1 = new State<_>(stream, -1)
    #endif
        let s11 = s1.Advance(10)
        check s11 cs 11 1

    let testSlowPath() =
        let cs =  Array.create 17 '_'
        // check end of block handling with multi-block CharStream (blockSize = 8, blockOverlap = 3)
        for c4 in testChars do
            cs.[4] <- c4
            for c5 in testChars do
                cs.[5] <- c5
                for c6 in testChars do
                    cs.[6] <- c6
                    for c7 in testChars do
                        cs.[7] <- c7
                        for c8 in testChars do
                            cs.[8] <- c8
                            use stream = createTestStreamCharStream cs
                            let s0 = (new State<_>(stream, -1))
                            check (s0.Advance(4)) cs 4 4
                            check (s0.Advance(5)) cs 5 4
                            check (s0.Advance(6)) cs 6 4

    let testArgumentChecking() =
        let N = 10
        let cs =  Array.create N '_'
        let css = new string(cs, 0, N)
        use stream = new CharStream(css, 0, cs.Length)
        let s0 = (new State<_>(stream, ()))

        let mutable nSkippedChars = 0
        let sEnd1 = s0.SkipCharsOrNewlines(System.Int32.MaxValue, &nSkippedChars)
        nSkippedChars |> Equal N
        int32 sEnd1.Index |> Equal N
        let mutable str = null
        let sEnd2 = s0.SkipCharsOrNewlines(System.Int32.MaxValue, &str)
        str |> Equal css

        try s0.SkipCharsOrNewlines(-1, &nSkippedChars) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try s0.SkipCharsOrNewlines(System.Int32.MinValue, &nSkippedChars) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try s0.SkipCharsOrNewlines(-1, &str) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try s0.SkipCharsOrNewlines(System.Int32.MinValue, &str) |> ignore; Fail()
        with :? System.ArgumentException -> ()

        let alwaysTrue = fun c -> true

        let sEnd3 = s0.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysTrue, -1, System.Int32.MaxValue)
        int32 sEnd3.Index |> Equal N
        let sEnd4 = s0.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysTrue, -1, System.Int32.MaxValue, &str)
        int32 sEnd4.Index |> Equal N
        str |> Equal css
        try s0.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysTrue, -1, -1) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try s0.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysTrue, -1, System.Int32.MinValue) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try s0.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysTrue, -1, -1, &str) |> ignore; Fail()
        with :? System.ArgumentException -> ()
        try s0.SkipCharsOrNewlinesWhile(alwaysTrue, alwaysTrue, -1, System.Int32.MinValue, &str) |> ignore; Fail()
        with :? System.ArgumentException -> ()

    let SkipCharsOrNewlinesWhileMinChars() =
        let cs = "0123456789"
        use stream = new CharStream(cs, 0, cs.Length)
        let s0 = new State<_>(stream, ())

        let smaller n = fun c -> int c < int '0' + n
        for n = 0 to 10 do
            let s1 = s0.SkipCharsOrNewlinesWhile(smaller n, smaller n, n, System.Int32.MaxValue)
            int32 s1.Index |> Equal n
            let mutable str = null
            let s2 = s0.SkipCharsOrNewlinesWhile(smaller n, smaller n, n, System.Int32.MaxValue, &str)
            int32 s2.Index |> Equal n

            let s3 = s0.SkipCharsOrNewlinesWhile(smaller n, smaller n, n + 1, System.Int32.MaxValue)
            s3 |> ReferenceEqual s0
            let s4 = s0.SkipCharsOrNewlinesWhile(smaller n, smaller n, n + 1, System.Int32.MaxValue, &str)
            s4 |> ReferenceEqual s0

            let s5 = s0.SkipCharsOrNewlinesWhile((fun c -> true), (fun c -> true), n + 1, n)
            s5 |> ReferenceEqual s0
            let s6 = s0.SkipCharsOrNewlinesWhile((fun c -> true), (fun c -> true), n + 1, n, &str)
            s6 |> ReferenceEqual s0

    let testIndexOffset() =
        use stream = new CharStream("\n___", 0, 4, 100L)
        let s0 = new State<_>(stream, (), "test")
        s0.Index      |> Equal 100L
        s0.LineBegin  |> Equal 100L
        s0.StreamName |> Equal "test"
        let s1 = s0.SkipNewline()
        s1.Index     |> Equal 101L
        s1.Line      |> Equal 2L
        s1.LineBegin |> Equal 101L
        s1.Column    |> Equal 1L
        s1.Position  |> Equal (Position("test", 101L, 2L, 1L))

    testFastPath()
    testSlowPath()
    testArgumentChecking()
    SkipCharsOrNewlinesWhileMinChars()
    testIndexOffset()

let testSkipToString() =
    // we already test the main skipping and maxChars logic in testSkipCharsOrNewlines
    let cs = "ABCDEFGHIJKLMNOPQRST".ToCharArray()
    use stream = createTestStreamCharStream cs // blockSize = 8, blockOverlap = 3
    let s0 = new State<_>(stream, ())
    for i0 = 0 to cs.Length - 1 do
        let si0 = s0.Advance(i0)
        for i1 = i0 to cs.Length - 1 do
           for n = 1 to cs.Length - i1 do
               let check strToFind maxChars isPresent =
                   let iEnd = if isPresent then i1
                              elif maxChars < cs.Length - i0 then i0 + maxChars
                              else cs.Length

                   let skippedString = if not isPresent then null
                                       else new string(cs.[i0..i1 - 1])

                   s0.Iter.Read() |> ignore; si0.Iter.Read() |> ignore
                   let mutable found = false
                   let s1 = si0.SkipToString(strToFind, maxChars, &found)
                   int32 s1.Index |> Equal iEnd
                   found |> Equal isPresent

                   s0.Iter.Read() |> ignore; si0.Iter.Read() |> ignore
                   let mutable str = null
                   let s2 = si0.SkipToString(strToFind, maxChars, &str)
                   int32 s2.Index |> Equal iEnd
                   str |> Equal skippedString

                   s0.Iter.Read() |> ignore; si0.Iter.Read() |> ignore
                   let strToFindCI = CharStream.FoldCase(strToFind)
                   let s3 = si0.SkipToStringCI(strToFindCI, maxChars, &found)
                   int32 s3.Index |> Equal iEnd
                   found |> Equal isPresent

                   s0.Iter.Read() |> ignore; si0.Iter.Read() |> ignore
                   let s4 = si0.SkipToStringCI(strToFindCI, maxChars, &str)
                   int32 s4.Index |> Equal iEnd
                   str |> Equal skippedString

               let strToFind = new string(cs, i1, n)
               check strToFind (i1 - i0) true
               if i1 - i0 > 0 then
                   check strToFind (i1 - i0 - 1) false
               if n > 1 then
                   let strToNotFind = string (char (int strToFind.[0] + 1)) + (if n > 1 then strToFind.Substring(1) else "")
                   check strToNotFind System.Int32.MaxValue false
                   let strToNotFind2 = strToFind.Substring(0, n - 1) + string (char (int strToFind.[n - 1] + 1))
                   check strToNotFind2 System.Int32.MaxValue false

    let testRemainingBranch() =
        use stream = new CharStream("1234567890")
        let state0 = new State<_>(stream, ())
        let mutable foundString = false
        state0.SkipToString("12345", System.Int32.MaxValue, &foundString) |> ReferenceEqual state0
        foundString |> Equal true

    testRemainingBranch()
    let mutable found = false
    let mutable str = null
    try s0.SkipToString(null, 10, &found) |> ignore; Fail()
    with :? System.NullReferenceException -> ()
    try s0.SkipToString(null, 10, &str) |> ignore; Fail()
    with :? System.NullReferenceException -> ()
    try s0.SkipToString("", 10, &found) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToString("", 10, &str) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToString("a", -1, &found) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToString("a", -1, &str) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToString("a", System.Int32.MinValue, &found) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToString("a", System.Int32.MinValue, &str) |> ignore; Fail()
    with :? System.ArgumentException -> ()

    try s0.SkipToStringCI(null, 10, &found) |> ignore; Fail()
    with :? System.NullReferenceException -> ()
    try s0.SkipToStringCI(null, 10, &str) |> ignore; Fail()
    with :? System.NullReferenceException -> ()
    try s0.SkipToStringCI("", 10, &found) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToStringCI("", 10, &str) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToStringCI("a", -1, &found) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToStringCI("a", -1, &str) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToStringCI("a", System.Int32.MinValue, &found) |> ignore; Fail()
    with :? System.ArgumentException -> ()
    try s0.SkipToStringCI("a", System.Int32.MinValue, &str) |> ignore; Fail()
    with :? System.ArgumentException -> ()

open FParsec.CharParsers

let testHelperParseSubstream() =
    use stream = new CharStream("1234567", 1, 6, 100L)
    let s0 = (new State<_>(stream, ())).Advance(2)
    let s1 = s0.Advance(3)
    let sEnd = s0.Advance(System.Int32.MaxValue)
    match runParserOnSubstream restOfLine -1 s0 s0 with
    | Success(result, ustate, pos) ->
        result |> Equal ""
        ustate |> Equal -1
        pos.Index |> Equal 102L
    | Failure(msg, _, _) -> Fail()
    match runParserOnSubstream restOfLine -1 sEnd sEnd with
    | Success(result, ustate, pos) ->
        result |> Equal ""
        ustate |> Equal -1
        pos.Index |> Equal 106L
    | Failure(msg, _, _) -> Fail()
    match runParserOnSubstream restOfLine -1 s0 s1 with
    | Success(result, ustate, pos) ->
        result |> Equal "456"
        ustate |> Equal -1
        pos.Index |> Equal 105L
    | Failure(msg, _, _) -> Fail()
    try runParserOnSubstream restOfLine -1 s1 s0 |> ignore; Fail()
    with :? System.ArgumentException -> ()

    let oldS0 = s0

#if LOW_TRUST
#else
    use stream = new CharStream("1234567".ToCharArray(), 1, 6)
    let s0 = (new State<_>(stream, ())).Advance(2)
    let s1 = s0.Advance(3)
    match runParserOnSubstream restOfLine -1 s0 s1 with
    | Success(result, ustate, pos) ->
        result |> Equal "456"
        pos.Index |> Equal 5L
    | Failure(msg, _, _) -> Fail()
#endif

    use stream = createTestStreamCharStream ("1234567890".ToCharArray()) // multi-block CharStream (blockSize = 8, blockOverlap = 3)
    let s0 = (new State<_>(stream, ())).Advance(2)
    let s1 = s0.Advance(5)
    match runParserOnSubstream restOfLine -1 s0 s1 with
    | Success(result, ustate, pos) ->
        result |> Equal "34567"
        pos.Index |> Equal 7L
    | Failure(msg, _, _) -> Fail()
    try runParserOnSubstream restOfLine -1 s1 s0 |> ignore; Fail()
    with :? System.ArgumentException -> ()

    let s2 = s0.Advance(10)  // the stream's block is now different from the one of s0 and s1
    match runParserOnSubstream restOfLine -1 s0 s1 with
    | Success(result, ustate, pos) ->
        result |> Equal "34567"
        pos.Index |> Equal 7L
    | Failure(msg, _, _) -> Fail()

    match runParserOnSubstream restOfLine -1 s0 s2 with
    | Success(result, ustate, pos) ->
        result |> Equal "34567890"
        pos.Index |> Equal 10L
    | Failure(msg, _, _) -> Fail()

    try runParserOnSubstream restOfLine -1 s2 s0 |> ignore; Fail()
    with :? System.ArgumentException -> ()

    try runParserOnSubstream restOfLine -1 oldS0 s0 |> ignore; Fail()
    with :? System.ArgumentException -> ()


let testHelperCountTextElements() =
    let countTextElementsRef s =
        let te = System.Globalization.StringInfo.GetTextElementEnumerator(s)
        let mutable count = 0
        while te.MoveNext() do count <- count + 1
        count

    let chars = [|"\u0020"; "\u007e"; "\U0001D41A";
                  "\u001F";" \u007F"; // control
                  "\u00AD"; "\U0001D173"; // format
                  "\ud800"; // surrogate
                  "\u0333"; "\U000101FD" // nonspacing mark
                  "\u0BBE"; "\U0001D166" // spacing combining mark
                  "\u20DD" // enclosing mark
                |]

    for c in chars do
        Helper.CountTextElements(c) |> Equal (countTextElementsRef c)

    for c1 in chars do
        for c2 in chars do
            let s = c1 + c2
            Helper.CountTextElements(s) |> Equal (countTextElementsRef s)

    let rand = System.Random()
    let strings = Array.zeroCreate 5
    for i = 0 to 100000 do
        for j = 0 to strings.Length - 1 do
           strings.[j] <- chars.[rand.Next()%chars.Length]
           let s = System.String.Concat(strings)
           Helper.CountTextElements(s) |> Equal (countTextElementsRef s)


let run () =
#if LOW_TRUST
#else
    setStaticField typeof<FParsec.CharStream> "DoNotRoundUpBlockSizeToSimplifyTesting" true
#endif
    testEmptyStream()
    testAdvance()
    testSkipWhitespace()
    testSkipRestOfLine()
    testSkipCharsOrNewlines()
    testSkipToString()
    testHelperParseSubstream()
    testHelperCountTextElements()
