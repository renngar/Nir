// Functions specific to the Nir.ini file.
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
