[<AutoOpen>]
module Nir.UI.Controls.TextBox

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

let textBox attributes text =
    TextBox.create [ TextBox.text text
                     yield! attributes ]
    |> Helpers.generalize

let acceptsReturn = TextBox.acceptsReturn
let acceptsTab = TextBox.acceptsTab
let onTextChanged = TextBox.onTextChanged
let isReadOnly = TextBox.isReadOnly
let textWrapping = TextBox.textWrapping
