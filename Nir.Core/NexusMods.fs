namespace Nir.Core

open System.IO
open FSharp.Data
open FSharp.Data.HttpRequestHeaders

open Utility.INI
open Utility.Path

module NexusMods =
    let private readIni (filePath: string) =
        try
            use sr = new StreamReader(filePath)
            sr.ReadToEnd()
        with :? System.IO.FileNotFoundException -> ""

    let getNexusApiKey() =
        getProgramPath() +/ "Nir.ini"
        |> readIni
        |> parseIni
        |> section "Nexus"
        |> property "ApiKey"

    type ValidateProvider = JsonProvider<"../Data/validate.json", RootName="Validate">

    let usersValidate apiKey =
        async {
            let! json = Http.AsyncRequest
                            ("https://api.nexusmods.com/v1/users/validate.json",
                             headers =
                                 [ "Accept", "application/json"
                                   "apikey", apiKey ])
            return match json.Body with
                   | Text s -> json.Headers, ValidateProvider.Parse(s)
                   | Binary data -> failwithf "Expected text, but got an %d byte binary response" data.Length
        }
