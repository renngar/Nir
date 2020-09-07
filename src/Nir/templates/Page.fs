module Nir.Page

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL


// Model

type Model = unit

let init = ()

// Update

type Msg = | None

let update (_: Msg) (model: Model): Model * Cmd<_> = model, Cmd.none

// View

let view (_: Model) (_: Msg -> unit) =
    DockPanel.create [ DockPanel.children [ TextBlock.create [ TextBlock.text "Your content here!" ] ] ]
