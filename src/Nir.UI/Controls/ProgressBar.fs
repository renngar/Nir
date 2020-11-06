[<AutoOpen>]

module Nir.UI.Controls.ProgressBar

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

let progressBar attributes =
    ProgressBar.create attributes
    |> Helpers.generalize

let isIndeterminate = ProgressBar.isIndeterminate

let maximum = ProgressBar.maximum
let value = ProgressBar.value
