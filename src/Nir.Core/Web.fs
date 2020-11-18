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
