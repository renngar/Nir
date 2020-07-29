module Nir.Pages.Start

open FSharp.Data

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open Nir.Controls
open Nir.Dialogs
// Model

type Model =
    { Window: Window
      File: string option }

let init window =
    { Window = window
      File = None }, Cmd.none

// Update

type Msg =
    | OpenLocalModList
    | AfterSelectFile of string
    | Loaded of HtmlDocument

type Link =
    { Text: string
      Link: string }

let update (msg: Msg) (model: Model): Model * Cmd<_> =
    match msg with
    | OpenLocalModList -> model, Cmd.OfAsync.perform promptHtmlFileName model.Window AfterSelectFile

    | AfterSelectFile file ->
        // HtmlDocument.AsyncLoad()
        // let results = HtmlDocument.Load(file)
        { model with File = Some file }, Cmd.OfAsync.perform HtmlDocument.AsyncLoad file Loaded

    | Loaded html ->
        let links =
            html.Descendants [ "a" ]
            |> Seq.choose (fun x ->
                x.TryGetAttribute("href")
                |> Option.map (fun a ->
                    { Text = x.InnerText()
                      Link = a.Value() }))
        links
        |> Seq.map (fun l -> printfn "%s %s" l.Text l.Link)
        |> ignore

        printfn "%A" links |> ignore

        model, Cmd.none



// View
let gridTextBlock row cls text =
    textBlock
        [ GridRow row
          Class cls ] text

let private theView (_: Model) (dispatch: Msg -> unit) =
    Grid.create
        [ Grid.margin 10.0
          Grid.rowDefinitions "auto, auto, *, auto"
          Grid.children
              [ gridTextBlock 0 "subtitle" "Nir lets you install Skyrim Mod Guides from the Web"
                Button.create
                    [ Grid.row 1
                      Button.content (textBlock [ Class "subtitle" ] "Open a local HTML modlist")
                      Button.onClick (fun _ -> dispatch OpenLocalModList) ]
                gridTextBlock 2 "" ""
                textBlock
                    [ GridRow 3
                      Class "link"
                      HorizontalAlignment HorizontalAlignment.Right
                      VerticalAlignment VerticalAlignment.Stretch ] "Advanced..." ] ]

let view (m: Model) (dispatch: Msg -> unit) = DockPanel.create [ DockPanel.children [ theView m dispatch ] ]
