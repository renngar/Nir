module Nir.TestUtils

open FSharp.Core

// Xeger
open Fare

open FParsec
open FsCheck

/// Returns a generator for strings matching the regex pattern
let genMatches pattern =
    Gen.sized (fun size ->
        let xeger = Xeger pattern

        match size with
        | 0 -> []
        | _ -> [ for _ in 1 .. size -> xeger.Generate() ]
        |> Gen.elements
        |> Gen.resize size)

let testPatterns rx f =
    Prop.forAll (Arb.fromGen (genMatches rx)) f

let inline parse p str = run p str

let parses p s =
    match parse p s with
    | Success _ -> true
    | Failure _ -> false

let parsesAs p s expected =
    match parse p s with
    | Success (result, _, _) -> result = expected
    | Failure _ -> false
