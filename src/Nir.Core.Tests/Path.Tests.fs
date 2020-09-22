module Nir.Core.Tests.Path

open System.IO

open Nir.Utility.Path

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``getProgramPath returns not empty`` () =
    getProgramPath () |> should not' (be EmptyString)

[<Fact>]
let ``program path exists`` () =
    getProgramPath ()
    |> Directory.Exists
    |> should be True
