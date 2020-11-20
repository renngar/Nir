// Nir DSL for Avalonia.FuncUI grids.
//
// Copyright (C) 2020 Renngar <renngar@renngar.com>
//
// This file is part of Nir.
//
// Nir is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <https://www.gnu.org/licenses/>.
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
