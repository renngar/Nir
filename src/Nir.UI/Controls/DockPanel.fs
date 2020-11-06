[<AutoOpen>]
module Nir.UI.Controls.DockPanel

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

let dockPanel attributes children =
    DockPanel.create [ DockPanel.children children
                       yield! attributes ]
    |> Helpers.generalize
