module Nir.Core.Tests.INI

open Fare                               // Xeger
open FParsec

open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit

open Nir.Utility.INI

let sectionPattern = "[^ \000\t\n\r\]]+"
let propertyNamePattern = "^[^;\\[\000\n\r=][^\000\n\r=]*"
let propertyLinePattern = "^[^;\\[\000\n\r=]+=[^\000\r\n]*[\r\n]*"

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


type StringFormat = Printf.StringFormat<string, string>

let parsesAsSectionFormat format s =
    parsesAs sectionHeader (sprintf format s) (IniSection s)

let parsesAsSection s = parsesAsSectionFormat "[%s]" s

let generate rx =
    (rx |> Xeger).Generate()
    |> Printf.StringFormat<string -> string>

[<Property>]
let ``Addition is commutative`` a b =
    a + b = b + a

[<Property>]
let ``Stringed floats parse as floats`` (NormalFloat n) =
    parsesAs pfloat (string n) n

[<Fact>]
let ``Section names cannot be empty`` () =
    parsesAsSection "" |> should be False

[<Fact>]
let ``Empty file parses`` () =
    parses iniFile "" |> should be True

[<Fact>]
let ``Empty property value parses`` () =
    parses propertyValue "\n"
    |> should be True

[<Fact>]
let ``Empty assignment value parses`` () =
    parses (assignment >>. propertyValue) "=\n"
    |> should be True

[<Property>]
let ``Strings without whitespace parse as valid section names`` () =
    testPatterns sectionPattern parsesAsSection

[<Property>]
let ``Section names cannot have whitespace`` () =
    testPatterns (sprintf "%s[ \t\n\r]%s+" sectionPattern sectionPattern)
                 (not << parsesAsSection)

[<Property>]
let ``Section names cannot have nulls`` () =
    testPatterns (sprintf "%s\000%s" sectionPattern sectionPattern)
        (not << parsesAsSection)

[<Property>]
let ``Section ignores left space padding`` () =
    testPatterns sectionPattern
        (parsesAsSectionFormat (generate "\[[ \t]+%s\]"))

[<Property>]
let ``Section ignores right space padding`` () =
    testPatterns sectionPattern (parsesAsSectionFormat (generate "\[%s \]"))

[<Property>]
let ``Section ignores space padding on both sides`` () =
    testPatterns sectionPattern
        (parsesAsSectionFormat (generate "\[[ \t]+%s[ \t]+\]"))

[<Property>]
let ``Whitespace only parses`` () =
    testPatterns "^[\n\r ][\n\r ]*" (parses iniFile)

[<Property>]
let ``Single character property name parses`` () =
    testPatterns propertyNamePattern (parses propertyName)

[<Property>]
let ``Property without a value parses`` () =
    testPatterns (sprintf "%s=[ \r\n]?" propertyNamePattern)
        (parses propertyLine)

[<Property>]
let ``Property lines parse`` () =
    testPatterns propertyLinePattern (parses propertyLine)

[<Property>]
let ``Property name does not end with a space`` () =
    testPatterns (sprintf "%s " propertyLinePattern) (fun s ->
        match parse propertyLine s with
        | Success (IniProperty { Name = n }, _, _) ->
            (not << Text.IsWhitespace) n.[n.Length - 1]
        | _ -> false)

[<Property>]
let ``Multiple sections parse`` () =
    testPatterns (sprintf "(\[%s\]\n)+" sectionPattern) (parses iniFile)

[<Property>]
let ``Multiple sections parse with random space in between`` () =
    testPatterns
        (sprintf "(\[%s\]\n[ \t\r\n]+)+" sectionPattern) (parses iniFile)
