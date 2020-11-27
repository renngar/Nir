// File path functions.
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
module Nir.Utility.Path

open System.IO
open System.Text.RegularExpressions

/// Custom operator for combining paths
let (+/) path1 path2 = Path.Combine(path1, path2)

/// Returns the root directory of the currently running program
///
/// For example if the executable is in C:\Program\bin\Debug\netcoreapp3.1, this
/// will return C:\Program.
let getProgramPath () =
    let tryMatch rx path =
        let m = Regex.Match(path, rx)
        if m.Success then m.Groups.[1].Value else path

    let root =
        System
            .Reflection
            .Assembly
            .GetExecutingAssembly()
            .CodeBase
        |> tryMatch @"^(.*)/bin" // Development Build
        |> tryMatch @"^(.*)/[^/]*\.(exe|dll)" // Published Release

    System.Uri(root).LocalPath

/// Return just the file name without any directories
let baseName path = FileInfo(path).Name
