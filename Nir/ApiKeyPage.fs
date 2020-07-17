module Nir.ApiKeyPage

open FSharp.Data

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open Nir.Controls
open Nir.Dialogs
// Model

type Model =
    { ApiKey: string }

let init =
    { ApiKey = "" }, Cmd.none

// Update

type Msg =
    | None


let update (msg: Msg) (model: Model): Model * Cmd<_> =
        model, Cmd.none

// View

let button (text: string) = Button.create [ Button.content text ]

let private theView (_: Model) (dispatch: Msg -> unit) =
    StackPanel.create
        [ StackPanel.children [
            button "Connect to Nexus"
            button "Enter API Key Manually"
            button "Disconnect from Nexus" ] ]

let view (m: Model) (dispatch: Msg -> unit) =
    DockPanel.create [ DockPanel.children [ theView m dispatch ] ]
