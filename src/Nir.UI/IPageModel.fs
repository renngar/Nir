// Interface for Nir UI pages.
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

/// How the page is treated by the history system as manifest through the back and forward buttons.
type HistoryStyle =
    /// Participate normally in the history system
    | Normal
    /// Does not get added to the history.
    ///
    /// Preserves the state behind the forward button, but disables it while active.
    | NoHistory
    /// Does not participate in history and disables navigation controls and menus.
    | Modal

type IPageModel =
    inherit IPluginModel
    abstract HistoryStyle: HistoryStyle
