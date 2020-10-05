[<AutoOpen>]
module Nir.UI.Controls.StackPanel

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let stackPanel attributes children =
    StackPanel.create [ StackPanel.children children
                        yield! attributes ] :> IView

/// Creates a StackPanel with the given class and children
let stackPanelCls ``class`` children = stackPanel [ cls ``class`` ] children

let orientation = StackPanel.orientation
