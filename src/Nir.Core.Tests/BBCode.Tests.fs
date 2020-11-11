module Nir.Tests.BBCode

open global.Xunit
open FsCheck.Xunit
open FsUnit.Xunit
open FsCheck // Must be after Xunit

open FParsec.CharParsers
open Nir.TestUtils
open Nir.Utility.BBCode.Parser

type Marker =
    class
    end

let private tagNamePattern = "[a-zA-Z0-9]+"
let private textPattern = "[^\[\]]*"
let private valuePattern = "[^\t\n\r\" ]]*"

let private urlPattern =
    @"http://[a-z]+\.[a-z]\.com(/[a-z0-9]+)*"

let private tagNameGenerator = genMatches tagNamePattern

let parses s =
    match runParserOnString document State.Default "" s with
    | Success _ -> true
    | Failure _ -> false

let private testBasicTag tag =
    testPatterns (sprintf @"%s+\[%s\]%s\[/%s\]%s" textPattern tag textPattern tag textPattern) parses

let private testStandaloneTag tag =
    testPatterns (sprintf @"%s\[%s\]%s" textPattern tag textPattern) parses

let private testValueTag tag valuePattern =
    testPatterns (sprintf @"%s\[%s=%s\]%s\[/%s\]%s" textPattern tag valuePattern textPattern tag textPattern) parses

[<Property>]
let ``Bold tag parses`` () = testBasicTag "b"

[<Property>]
let ``Underline tag parses`` () = testBasicTag "u"

[<Property>]
let ``Italic tag parses`` () = testBasicTag "i"

[<Property>]
let ``Strikethrough tag parses`` () = testBasicTag "s"

[<Property>]
let ``Font face tag parses`` () =
    testValueTag "font" "\\\"[a-zA-Z][a-zA-Z ]*\\\""

[<Property>]
let ``Unquoted font face tag parses`` () = testValueTag "font" "[a-zA-Z][a-zA-Z ]+"

[<Property>]
let ``Named color tag parses`` () = testValueTag "color" "[a-zA-Z]+"

[<Property>]
let ``Hex color tag parses`` (PositiveInt n) =
    // Xeger does something with the pound sign '#".  Escape it.
    testValueTag "color" (sprintf @"\#[0-9A-Fa-f]{%d}" [| 3; 6; 8 |].[n % 3])

[<Property>]
let ``Heading tag parses`` () = testBasicTag "heading"

[<Property>]
let ``Line tag parses`` () = testStandaloneTag "line"

[<Fact>]
let ``Unordered list parses`` () =
    parses """[list]
[*] Item
[*] Another
[/list]
"""
    |> should be True


[<Fact>]
let ``Ordered list parses`` () =
    parses """[list=1]
[*] Item
[*] Another
[/list]
"""
    |> should be True

[<Property>]
let ``Center tag parses`` () = testBasicTag "center"

[<Property>]
let ``Right-aligned tag parses`` () = testBasicTag "right"

[<Property>]
let ``Hyperlink tag parses`` () = testValueTag "url" urlPattern

[<Property>]
let ``Nested links should not parse`` () =
    testPatterns textPattern (fun s -> not (parses (sprintf "[a-zA-Z ]+[url]%s[url]%s[/url]%s[/url][a-zA-Z ]+" s s s)))

[<Property>]
let ``Quote tag parses`` () = testBasicTag "quote"

[<Property>]
let ``Image tag parses`` () = testBasicTag "img"

[<Property>]
let ``Aligned image tag parses`` (PositiveInt n) =
    testValueTag "img" ([| "left"; "center"; "right" |].[n % 3])

[<Property>]
let ``Youtube tag parses`` () = testBasicTag "youtube"

[<Property>]
let ``Spoiler tag parses`` () = testBasicTag "spoiler"
