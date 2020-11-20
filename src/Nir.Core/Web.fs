// Web-related functions.
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
module Nir.Web

open System.Diagnostics
open System.Runtime.InteropServices
open System.Text
open FSharp.Data

let openUrl url =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    then Process.Start(ProcessStartInfo("cmd", sprintf "/c start %s" url))
    elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
    then Process.Start("xdg-open", url)
    elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
    then Process.Start("open", url)
    else failwith "Unsupported OS Platform"
    |> ignore

let stripHtml text =
    let rec stripList (sb: StringBuilder) (nodes: seq<HtmlNode>) =
        nodes
        |> Seq.iter (fun node ->
            let descendants = node.Descendants()
            if Seq.isEmpty descendants then sb.Append(node.InnerText()) |> ignore else stripList sb descendants)

    let tryStrip str =
        let sb = StringBuilder()
        let nodes = HtmlNode.Parse(str)
        stripList sb nodes
        sb.ToString()

    try
        tryStrip text
    with _ ->
        // If the raw text doesn't parse, which can happen for an HTML fragment, wrap it in a body tag and try again.
        tryStrip (sprintf "<body>%s</body>" text)
