[<AutoOpen>]
module Nir.UI.Controls.ComboBox

open Avalonia.FuncUI.Types
open Avalonia.Controls
open Avalonia.FuncUI.DSL

/// Create a combobox
let comboBox attributes = ComboBox.create attributes :> IView
