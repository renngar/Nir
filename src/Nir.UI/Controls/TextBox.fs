[<AutoOpen>]
module Nir.UI.Controls.TextBox

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let textBox attributes text =
    TextBox.create [ TextBox.text text
                     yield! attributes ] :> IView

let acceptsReturn = TextBox.acceptsReturn
let acceptsTab = TextBox.acceptsTab
let onTextChanged = TextBox.onTextChanged
let textWrapping = TextBox.textWrapping
