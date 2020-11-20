// Interface for Nir's theme switcher.
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

namespace Nir.UI

type IThemeSwitcher =
    /// Is the light theme active?
    ///
    /// This may be used by UI components to display a selector.
    abstract IsLight: bool

    /// Toggles between styles.
    abstract Toggle: unit -> unit

    /// Loads plugin styles.
    ///
    /// Should be called when a plugin is made active.
    abstract LoadPluginStyles: IPlugin -> unit
