[<AutoOpen>]
module Nir.UI.Controls.TextBlock

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

let textBlock attributes text =
    TextBlock.create [ TextBlock.text text
                       yield! attributes ]
    |> Helpers.generalize

/// Creates a TextBlock with the given class and text
let textBlockCls ``class`` text = textBlock [ cls ``class`` ] text

let onTapped fn = TextBlock.onTapped fn
