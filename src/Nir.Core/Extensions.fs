module Nir.Extensions

/// Alias for `FSharp.Core.Ok` since `FParsec` defines its own `Ok`.
let FsOk = FSharp.Core.Ok
/// Alias for `FSharp.Core.Error` since `FParsec` defines its own `Error`.
let FsError = FSharp.Core.Error

/// Extensions to the Result module
module Result =
    /// Returns the contained value if the result is Ok.  Otherwise, throws an exception
    let succeeded result =
        match result with
        | FSharp.Core.Ok result' -> result'
        | FSharp.Core.Error msg -> invalidArg "result" (sprintf "Failed with \"%A\"" msg)

module List =
    /// Checks if the given list is sorted using keys given by the given projection. Keys are compared using
    /// `Operators.compare`.
    let rec isSortedBy projection list =
        match list with
        | []
        | [ _ ] -> true
        | x :: y :: xs ->
            Operators.compare (projection x) (projection y)
            <= 0
            && isSortedBy projection (y :: xs)
