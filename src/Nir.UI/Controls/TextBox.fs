// Nir DSL for Avalonia.FuncUI TextBoxes.
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
