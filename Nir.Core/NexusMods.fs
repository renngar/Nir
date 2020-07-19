module Nir.NexusMods

open FSharp.Data

open Utility.INI
open Utility.Path

let getNexusApiKey() =
    getProgramPath() +/ "Nir.ini"
    |> parseIniFile
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
