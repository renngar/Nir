module Nir.Core

open System.Text.RegularExpressions

let getProgramPath () =
    let root =
        System.Reflection.Assembly.GetExecutingAssembly().CodeBase
        |> (fun path -> Regex.Match(path, @"^(.*)/bin").Groups.[1].Value)
    let uri = System.Uri(root)
    uri.LocalPath
