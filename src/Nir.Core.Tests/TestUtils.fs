// General testing helpers
//
// Copyright (C) 2020 Renngar <renngar@renngar.com>
//
// This file is part of Nir.
//
// Nir is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <https://www.gnu.org/licenses/>.
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
