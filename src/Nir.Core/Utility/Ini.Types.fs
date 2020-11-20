// Types for interacting with .ini files.
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
        let spaces = many1Chars aSpace

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
        | Failure (_) ->
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

/// Functions that operate on `Properties`
module Properties =
    let areSorted properties =
        properties
        |> List.isSortedBy (fun p -> p.Property)

/// An INI file section including its name, properties and any preceding comments
type Section =
    { Sorted: bool
      Comments: string list
      Section: SectionName
      Properties: Properties }

/// A list of INI Sections for an .ini file
type Sections = Section list

/// Functions that operate on `Sections`
module Sections =
    let areSorted sections =
        sections |> List.isSortedBy (fun p -> p.Section)

/// An INI file including its sections with their properties and any trailing comments
type Ini =
    { Sorted: bool
      FileName: string
      Sections: Sections
      TrailingComments: string list }
