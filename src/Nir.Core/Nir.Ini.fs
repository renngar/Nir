module Nir.Ini

open Nir.Parsing
open Nir.Utility.INI

let private nexusSection = create<SectionName> "Nexus"

let private apiKeyProp = "ApiKey"

/// Get the Nexus Mods API Key, if any, from the ini
let nexusApiKey ini: IniPropertyValue * Ini =
    ini
    |> section nexusSection
    |> property apiKeyProp
    |> propertyValue

let setNexusApiKey ini value =
    setIniProperty ini nexusSection apiKeyProp value
