module Nir.Utility.Path

open System.Text.RegularExpressions

/// Custom operator for combining paths
let ( +/ ) path1 path2 = System.IO.Path.Combine(path1, path2)

/// Returns the root directory of the currently running program
///
/// For example if the executable is in C:\Program\bin\Debug\netcoreapp3.1, this
/// will return C:\Program.
let getProgramPath () =
    let root =
        System.Reflection.Assembly.GetExecutingAssembly().CodeBase
        |> (fun path -> Regex.Match(path, @"^(.*)/bin").Groups.[1].Value)
    let uri = System.Uri(root)
    uri.LocalPath
