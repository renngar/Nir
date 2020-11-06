[<AutoOpen>]
module Nir.UI.Controls.ListBox

open Avalonia.FuncUI
open Avalonia.Controls
open Avalonia.FuncUI.DSL

/// Create a ListBox
let listBox attributes =
    ListBox.create attributes |> Helpers.generalize

// Selection settings
let selectedItem = ListBox.selectedItem
let onSelectedIndexChanged = ListBox.onSelectedIndexChanged
let onSelectedItemChanged = ListBox.onSelectedItemChanged
