[<AutoOpen>]
module Nir.UI.Controls.StackPanel

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

let stackPanel attributes children =
    StackPanel.create [ StackPanel.children children
                        yield! attributes ]
    |> Helpers.generalize

/// Creates a StackPanel with the given class and children
let stackPanelCls ``class`` children = stackPanel [ cls ``class`` ] children

let orientation = StackPanel.orientation
