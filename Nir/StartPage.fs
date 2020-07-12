module Nir.StartPage

open Avalonia.FuncUI.DSL
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

let update (msg: Msg) (model: Model): Model * Cmd<_> =
    match msg with
    | OpenLocalModList ->
        model,
        Cmd.OfAsync.perform promptHtmlFileName model.Window AfterSelectFile

    | AfterSelectFile file ->
        { model with File = Some file }, Cmd.none

// View

let view (_: Model) (dispatch: Msg -> unit) =
    Grid.create
        [ Grid.margin 10.0
          Grid.rowDefinitions "auto, auto, *, auto"
          Grid.children
              [ textBlock 0 "subtitle"
                    "Nir lets you install Skyrim Mod Guides from the Web"
                Button.create
                    [ Grid.row 1
                      Button.content
                          (TextBlock.create
                              [ TextBlock.classes [ "subtitle" ]
                                TextBlock.text "Open a local HTML modlist" ])
                      Button.onClick (fun _ -> dispatch OpenLocalModList) ]
                textBlock 2 "" ""
                textBlockEx 3 "link" "Advanced..."
                    [ TextBlock.horizontalAlignment HorizontalAlignment.Right
                      TextBlock.verticalAlignment VerticalAlignment.Stretch ] ] ]
