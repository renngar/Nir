[<AutoOpen>]
module Nir.UI.Controls.DockPanel

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let dockPanel attributes children =
    DockPanel.create [ DockPanel.children children
                       yield! attributes ] :> IView
