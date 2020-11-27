// Nexus Mod Checker.
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
module Nir.Parsing

open FParsec

// Useful for debugging parsers, but disappears in a release build
#if DEBUG
let mutable private depth = 0

let (<!>) (p: Parser<_, _>) label: Parser<_, _> =
    fun stream ->
        let indent =
            Seq.replicate depth "  " |> String.concat ""

        depth <- depth + 1

        printfn "%A: %sEntering %s" stream.Position indent label
        let reply = p stream
        printfn "%A: %sLeaving %s (%A)" stream.Position indent label reply.Status
        depth <- depth - 1
        reply
#else
let inline (<!>) (p: Parser<_, _>) _label: Parser<_, _> = p
#endif

/////////////////
// Classifiers //
/////////////////

/// Returns `true` for the space char ' ' and `false` for all other chars.
let private isSpace c = c = ' '

/// Returns `true` for any ASCII letter or digit and `false` for all other chars.
let isWordChar c = isAsciiLetter c || isDigit c

/// Is the character a space ' ' or tab '\t'?
let inline isLineSpace c = c = ' ' || c = '\t'

/////////////
// Parsers //
/////////////

/// Parse and skips a sequence of any whitespace
let ws = spaces

/// Parses the space char ' ' and returns it.
let aSpace c = pchar ' ' c

/// Parses the double-quote character '"' and returns it.
let quote c = pchar '"' c

/// Parses a left, square bracket character '[' and returns it.
let openBracket c = pchar '[' c

/// Parses a right, square bracket character ']' and returns it.
let closeBracket c = pchar ']' c

/// Parses a sequence of spaces ' ' and tabs '\t'. Returns them as a string.
let lineWs c = manyChars (satisfy isLineSpace) c

/// Parses any char in the range '0' - '9', 'a' - 'z' and 'A' - 'Z'. Returns the parsed char.
let wordChar c = (asciiLetter <|> digit) c

/// Parses any series of characters in the range '0' - '9', 'a' - 'z' and 'A' - 'Z'. Returns the parsed string.
let wordChars s = many1Chars wordChar s

/// Parses a string of characters matched by `firstOrLast` allowing spaces in the middle of the string
let allowInternalSpaces (firstOrLast: Parser<char, 'a>): Parser<string, 'a> =
    /// Parses a sequence of spaces. They may occur in the string, but not at the beginning or end.
    let spaces = many1Chars aSpace

    /// Parses spaces followed by a character that can appear at the end of the string
    let safeSpaces =
        spaces .>>.? firstOrLast
        |>> (fun (spaces, c) -> spaces + string c)

    /// Parses a sequence that can safely appear at the end of the string
    let endingSequence = safeSpaces <|> many1Chars firstOrLast

    firstOrLast .>>. manyStrings endingSequence
    .>> lineWs
    |>> (fun (c, strings) -> string c + strings)

/// The parser `quoted p` applies the parser `between quote quote p`. It returns the result of `p`.
let quoted p = between quote quote p

/// The parser `bracketed p` applies the parser `between openBracket closeBracket p` looking for "[<p>]".  It returns
/// the result of `p`.
let bracketed p = between openBracket closeBracket p

/// Creates an object of type `T` using its `Create` method, which returns a `Result`.
///
/// Throws a `System.ArgumentException` if `Create` returns an `Error`.
let inline create< ^T when ^T: (static member Create: string -> Result< ^T, string >)> str =
    match (^T: (static member Create: string -> Result< ^T, string >) str) with
    | FSharp.Core.Ok s -> s
    | FSharp.Core.Error msg -> invalidArg "str" msg
