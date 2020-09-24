[<AutoOpen>]

module Nir.UI.Controls.ProgressBar

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let progressBar attributes = ProgressBar.create attributes :> IView

let isIndeterminate = ProgressBar.isIndeterminate

let maximum = ProgressBar.maximum
let value = ProgressBar.value
