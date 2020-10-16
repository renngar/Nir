module Nir.Tests.INI

open System
open FSharp.Core
open Fare // Xeger
open FParsec

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open Nir.Parsing
open Nir.Utility.INI
open Parser

let sectionPattern = "[a-zA-Z0-9][a-zA-Z0-9 ]*[a-zA-Z0-9]"
let propertyNamePattern = "^[^;\\[\000\n\r=][^\000\n\r=]*"

// Make sure there is at least one usable character in the property name
let propertyLinePattern =
    "^[^;\\[\000\n\r=]*[^ ;\\[\000\n\r=][^;\\[\000\n\r=]*=[^\000\r\n]*[\r\n]*"

let inline parse p str = run p str

let parses p s =
    match parse p s with
    | Success _ -> true
    | Failure _ -> false

let parsesAs p s expected =
    match parse p s with
    | Success (result, _, _) -> result = expected
    | Failure _ -> false

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
    rx
    |> genMatches
    |> Arb.fromGen
    |> Prop.forAll
    <| f

let parsesAsSectionFormat format s =
    let expected = IniSection(create<SectionName> s)
    parsesAs sectionHeader (sprintf format s) expected

let parsesAsSection s = parsesAsSectionFormat "[%s]" s

let generate rx =
    (rx |> Xeger).Generate()
    |> Printf.StringFormat<string -> string>

/// Lazy function `f` is expected to throw an `ArgumentException`.
let badArg f = Prop.throws<ArgumentException, _> f

[<Property>]
let ``Addition is commutative`` a b = a + b = b + a

[<Property>]
let ``Stringed floats parse as floats`` (NormalFloat n) = parsesAs pfloat (string n) n

[<Fact>]
let ``Section names cannot be empty`` () = badArg (lazy (parsesAsSection ""))

[<Fact>]
let ``Empty file parses`` () = parses iniFile "" |> should be True

[<Fact>]
let ``Empty property value parses`` () =
    parses propertyValue "\n" |> should be True

[<Fact>]
let ``Empty assignment value parses`` () =
    parses (assignment >>. propertyValue) "=\n"
    |> should be True

let sectionGenerator = genMatches sectionPattern

type SectionWithControlChar() =
    static member Generate() =
        let ctrlGen = Gen.choose (0, 20) |> Gen.map char

        Gen.map3 (fun pre c post -> pre + (string c) + post) sectionGenerator ctrlGen sectionGenerator
        |> Arb.fromGen

[<Property(Arbitrary = [| typeof<SectionWithControlChar> |])>]
let ``Section names cannot have control characters`` (c) =
    badArg (lazy (c |> (not << parsesAsSection)))

[<Property>]
let ``Section names can have internal spaces`` () =
    testPatterns (sprintf @"%s %s+" sectionPattern sectionPattern) parsesAsSection

[<Property>]
let ``Section ignores left space padding`` () =
    testPatterns sectionPattern (parsesAsSectionFormat (generate "\[[ \t]+%s\]"))

[<Property>]
let ``Section ignores right space padding`` () =
    testPatterns sectionPattern (parsesAsSectionFormat (generate "\[%s[ \t]+\]"))

[<Property>]
let ``Section ignores space padding on both sides`` () =
    testPatterns sectionPattern (parsesAsSectionFormat (generate "\[[ \t]+%s[ \t]+\]"))

[<Property>]
let ``Whitespace only parses`` () =
    testPatterns "^[\n\r ]+" (parses iniFile)

[<Property>]
let ``Single character property name parses`` () =
    testPatterns propertyNamePattern (parses propertyName)

[<Property>]
let ``Property without a value parses`` () =
    testPatterns (sprintf "%s=[ \r\n]?" propertyNamePattern) (parses propertyLine)

[<Property>]
let ``Property lines parse`` () =
    testPatterns propertyLinePattern (parses propertyLine)

[<Property>]
let ``Property name does not end with a space`` () =
    testPatterns (sprintf "%s " propertyLinePattern) (fun s ->
        match parse propertyLine s with
        | Success (IniProperty { Name = n }, _, _) -> (not << Text.IsWhitespace) n.[n.Length - 1]
        | _ -> false)

[<Property>]
let ``Multiple sections parse`` () =
    testPatterns (sprintf "(\[%s\]\n)+" sectionPattern) (parses iniFile)

[<Property>]
let ``Multiple sections parse with random space in between`` () =
    testPatterns (sprintf "(\[%s\]\n[ \t\r\n]+)+" sectionPattern) (parses iniFile)
