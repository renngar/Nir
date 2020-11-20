// General attributes for Nir's Avalonia.FuncUI DSL.
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
