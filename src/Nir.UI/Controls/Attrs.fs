[<AutoOpen>]
module Nir.UI.Controls.Attrs

open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL

/// Assigns a list of classes to a control
let inline classes (classes: string list) = TemplatedControl.classes classes

/// Assigns a single class to a control
let inline cls ``class`` = classes [ ``class`` ]

// Alignment settings
let horizontalAlignment = TemplatedControl.horizontalAlignment
let verticalAlignment = TemplatedControl.verticalAlignment

// Docking settings
let dock = TemplatedControl.dock

// Miscellaneous settings
let isEnabled = TemplatedControl.isEnabled
