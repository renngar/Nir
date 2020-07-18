namespace Nir.Core

open System.IO

open Utility.INI
open Utility.Path

module NexusMods =
    let private readIni (filePath: string) =
        try
            use sr = new StreamReader(filePath)
            sr.ReadToEnd()
        with
            | :? System.IO.FileNotFoundException -> ""

    let getNexusApiKey () =
        getProgramPath () +/ "Nir.ini"
        |> readIni
        |> parseIni
        |> section "Nexus" |> property "ApiKey"
