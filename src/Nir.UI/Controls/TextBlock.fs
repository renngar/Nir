[<AutoOpen>]
module Nir.UI.Controls.TextBlock

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let textBlock attributes text =
    TextBlock.create [ TextBlock.text text
                       yield! attributes ] :> IView

/// Creates a TextBlock with the given class and text
let textBlockCls ``class`` text = textBlock [ cls ``class`` ] text
