module Nir.Utility.Path

open System.Text.RegularExpressions

/// Custom operator for combining paths
let (+/) path1 path2 = System.IO.Path.Combine(path1, path2)

/// Returns the root directory of the currently running program
///
/// For example if the executable is in C:\Program\bin\Debug\netcoreapp3.1, this
/// will return C:\Program.
let getProgramPath() =
    let tryMatch rx path =
        let m = Regex.Match(path, rx)
        if m.Success then m.Groups.[1].Value else path

    let root =
        System.Reflection.Assembly.GetExecutingAssembly().CodeBase
        |> tryMatch @"^(.*)/bin" // Development Build
        |> tryMatch @"^(.*)/[^/]*\.(exe|dll)" // Published Release

    System.Uri(root).LocalPath
