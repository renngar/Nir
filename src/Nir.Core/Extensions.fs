// Extensions to F# types and modules.
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
