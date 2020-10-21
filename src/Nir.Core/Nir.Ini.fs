module Nir.Ini

open Nir.Parsing
open Nir.Utility.INI

let private generalSection = create<SectionName> "General"
let private themeProp = "Theme"
let private nexusSection = create<SectionName> "Nexus"
let private apiKeyProp = "ApiKey"

/// Get the Nexus Mods API Key, if any, from the ini
let nexusApiKey ini =
    getIniProperty ini nexusSection apiKeyProp

let setNexusApiKey ini value =
    setIniProperty ini nexusSection apiKeyProp value

type Theme =
    | Light = 0
    | Dark = 1

/// Get the theme type, if any, from the ini.  Defaults to `Light`.
let theme ini: Theme =
    getIniProperty ini generalSection themeProp
    |> fun x ->
        match x.ToLower() with
        | "dark" -> Theme.Dark
        | _ -> Theme.Light

let setTheme ini value =
    if value = Theme.Light then "Light" else "Dark"
    |> setIniProperty ini generalSection themeProp
