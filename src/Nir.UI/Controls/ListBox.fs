[<AutoOpen>]
module Nir.UI.Controls.ListBox

open Avalonia.FuncUI.Types
open Avalonia.Controls
open Avalonia.FuncUI.DSL

/// Create a ListBox
let listBox attributes = ListBox.create attributes :> IView

// Selection settings
let selectedItem = ListBox.selectedItem
let onSelectedIndexChanged = ListBox.onSelectedIndexChanged
let onSelectedItemChanged = ListBox.onSelectedItemChanged
