// Nir DSL for Avalonia.FuncUI TabControls.
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
module Nir.UI.Controls.TabControl

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

let tabControl attributes viewItems =
    TabControl.create [ TabControl.viewItems viewItems
                        yield! attributes ]
    |> Helpers.generalize

let tabControlCls ``class`` viewItems = tabControl [ cls ``class`` ] viewItems

let tabStripPlacement placement = TabControl.tabStripPlacement placement