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
let private isWordChar c = isAsciiLetter c || isDigit c

/// Is the character a space ' ' or tab '\t'?
let inline isLineSpace c = c = ' ' || c = '\t'

/////////////
// Parsers //
/////////////

/// Parses the space char ' ' and returns it.
let space c = satisfy ((=) ' ') c

/// Parses a sequence of spaces ' ' and tabs '\t'. Returns them as a string.
let lineWs c = manyChars (satisfy isLineSpace) c

/// Parses any char in the range '0' - '9', 'a' - 'z' and 'A' - 'Z'. Returns the parsed char.
let wordChar c = (asciiLetter <|> digit) c

/// Creates an object of type `T` using its `Create` method, which returns a `Result`.
///
/// Throws a `System.ArgumentException` if `Create` returns an `Error`.
let inline create< ^T when ^T: (static member Create: string -> Result< ^T, string >)> str =
    match (^T: (static member Create: string -> Result< ^T, string >) str) with
    | FSharp.Core.Ok s -> s
    | FSharp.Core.Error msg -> invalidArg "str" msg
