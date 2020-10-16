[<AutoOpen>]
module Nir.Utility.INI.Types

open FParsec
open Nir.Extensions
open Nir.Parsing

[<Struct>]
type SectionName =
    private { SectionName: string }

    /// Return the string value inside a SectionName
    member this.Value = this.SectionName

    /// Parses an INI section name.
    static member Parser: Parser<SectionName, unit> =
        /// Parses punctuation that is allowed in a `SectionName`.
        let allowedPunctuation = anyOf @"!$%&'()\*+,-./:<>?@\\^_`{|}~"

        /// Parses characters that are allowed at the beginning or the end of a `SectionName`.
        let firstOrLast = wordChar <|> allowedPunctuation

        /// Parses a sequence of spaces. They may occur in a `SectionName`, but not at the beginning or end.
        let spaces = many1Chars space

        /// Parses spaces followed by a character that can appear at the end of a section name
        let safeSpaces =
            spaces
            .>>.? firstOrLast
            |>> (fun (spaces, c) -> spaces + string c)

        /// Parses a sequence that can safely appear at the end of a SectionName
        let endingSequence = safeSpaces <|> many1Chars firstOrLast

        firstOrLast
        .>>. manyStrings endingSequence
        .>> lineWs
        |>> (fun (c, strings) -> { SectionName = (string c + strings) })

    /// Constructor
    static member Create str =
        match run (SectionName.Parser .>> eof) str with
        | Success (s, _, _) -> FsOk s
        | Failure (_, _, _) ->
            let msg =
                sprintf "Invalid INI section name: \"%s\"" str

            FsError msg

type IniPropertyValue = string

/// An INI file property including its name, value and any preceding comments
type Property =
    { Comments: string list
      Property: string
      Value: IniPropertyValue }

/// A list of INI Property settings that go in a Section
type Properties = Property list

/// An INI file section including its name, properties and any preceding comments
type Section =
    { Comments: string list
      Section: SectionName
      Properties: Properties }

/// An INI file including its sections with their properties and any trailing comments
type Ini =
    { FileName: string
      Sections: Section list
      TrailingComments: string list }
