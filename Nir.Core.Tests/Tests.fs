module Nir.Core.Tests.Tests

open Xunit
open Nir.Core
open System.IO
open FsUnit.Xunit

[<Fact>]
let ``getProgramPath returns not empty`` () =
    getProgramPath ()
    |> should not' (be EmptyString)

[<Fact>]
let ``program path exists`` () =
    getProgramPath ()
    |> Directory.Exists
    |> should be True
