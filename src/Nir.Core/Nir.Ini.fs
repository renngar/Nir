module Nir.Ini

open Nir.Parsing
open Nir.Utility.INI

let private NexusSection = create<SectionName> "Nexus"

let private ApiKeyProp = "ApiKey"

/// Get the Nexus Mods API Key, if any, from the ini
let nexusApiKey ini: IniPropertyValue * Ini =
    ini
    |> section NexusSection
    |> property ApiKeyProp
    |> propertyValue

let setNexusApiKey ini value =
    setIniProperty ini NexusSection ApiKeyProp value
