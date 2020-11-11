namespace Nir.Core.Tests

open FParsec.CharParsers
open FsCheck
open Nir.Tests
open Nir.Tests.BBCode
open Nir.Utility.BBCode.Parser

module Main =
    [<EntryPoint>]
    let main _ =
        // Arb.registerByType (typeof<Marker>.DeclaringType) |> ignore
//        Check.QuickAll(typeof<Marker>.DeclaringType)
        // Check.Quick ``Hex color tag parses``

        printfn "%A"
        <| runParserOnString document State.Default "" """[list=1]
[*] Item 1
[*] Item 2
[/list]
"""
        0
