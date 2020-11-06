[<AutoOpen>]
module Nir.UI.Controls.ComboBox

open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

/// Create a combobox
let comboBox attributes =
    ComboBox.create attributes |> Helpers.generalize
