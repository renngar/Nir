[<AutoOpen>]
module Nir.UI.Controls.StackPanel

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let stackPanel attributes children =
    StackPanel.create [ StackPanel.children children
                        yield! attributes ] :> IView

let orientation = StackPanel.orientation
let spacing = StackPanel.spacing
