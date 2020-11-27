// Entry point for Nir.
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
namespace Nir

open System

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI

/// This is your application you can ose the initialize method to load styles
/// or handle Life Cycle events of your application
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Nir/Styles/LightTheme.xaml"
        this.Name <- "Nir"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let window = Shell.MainWindow()
            window.Icon <- WindowIcon(Assets.Open("Icons/Nir.ico"))
            desktopLifetime.MainWindow <- window
        | _ -> ()

module Program =

    [<STAThread>] // Required for Drag and drop from an external source
    [<EntryPoint>]
    let main (args: string []) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
