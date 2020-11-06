[<AutoOpen>]
module Nir.UI.Controls.Grid

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

/// Create a Grid
let grid attributes children =
    Grid.create [ Grid.children children
                  yield! attributes ]
    |> Helpers.generalize

let column = Grid.column
let columnDefinitions (defs: ColumnDefinitions) = Grid.columnDefinitions defs

let toColumnDefinitions s =
    s |> ColumnDefinitions.Parse |> columnDefinitions

let columnSpan = Grid.columnSpan

let row = Grid.row
let rowSpan = Grid.rowSpan

let rowDefinitions (defs: RowDefinitions) = Grid.rowDefinitions defs

let toRowDefinitions s =
    s |> RowDefinitions.Parse |> rowDefinitions
