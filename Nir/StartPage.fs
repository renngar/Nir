module Nir.StartPage

open Avalonia.FuncUI.DSL
open Elmish
open Avalonia.Controls
open Nir.Controls
open Avalonia.Layout

// Model

type Model =
    { Window: Window
      File: string option }

let init window =
    { Window = window
      File = None },
    Cmd.none

// Update

type Msg =
    | OpenLocalModList
    | AfterSelectFile of string[]

let update (msg: Msg) (model: Model) =
    match msg with
    | OpenLocalModList ->
        let dialog = Dialogs.getHtmlFileDialog None
        let showDialog window = dialog.ShowAsync(window) |> Async.AwaitTask
        model, Cmd.OfAsync.perform showDialog model.Window AfterSelectFile
    | AfterSelectFile files ->
        { model with File = (Array.tryExactlyOne files) },
        Cmd.none
    
// View
    
let view (_: Model) (dispatch: Msg -> unit) =
    Grid.create [
        Grid.margin 10.0
        Grid.rowDefinitions "auto, auto, *, auto"
        Grid.children [
            textBlock 0 "subtitle" "Nir lets you install Skyrim Mod Guides from the Web"
            Button.create [
                Grid.row 1
                Button.content (TextBlock.create [
                    TextBlock.classes [ "subtitle" ]
                    TextBlock.text "Open a local HTML modlist" ] )
                Button.onClick (fun _ -> dispatch OpenLocalModList) ]
            textBlock 2 "" ""
            textBlockEx 3 "link" "Advanced..." [
                TextBlock.horizontalAlignment HorizontalAlignment.Right
                TextBlock.verticalAlignment VerticalAlignment.Stretch ] ] ]
