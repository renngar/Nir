// Theme switcher for the Nir UI.
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
open Elmish
open Avalonia
open Avalonia.FuncUI
open Avalonia.Styling
open Avalonia.Markup.Xaml.Styling
open Nir.Ini
open Nir.UI

type private ThemeSwitcher(styles: Styles, theme: Theme) as this =
    /// Converts style paths to `StyleIncludes`.
    let toStyles =
        List.map (fun source ->
            let style = StyleInclude(baseUri = null)
            style.Source <- Uri(source)
            style)

    /// The light and dark system styles
    let systemStyles =
        [ "avares://Nir/Styles/LightTheme.xaml"
          "avares://Nir/Styles/DarkTheme.xaml" ]
        |> toStyles

    /// The light and dark styles for the current plugin, if any.
    let mutable pluginStyles = []

    /// The current theme selection.
    let mutable theme = theme

    /// Functions that are subscribed to theme changes.
    let mutable subscribers = []

    /// Index of the current theme into `systemStyles` and `pluginStyles`.
    let styleIndex () = int theme

    /// Are there any plugin styles in the actual Avalonia Window Styles array?
    let havePluginStyle () = styles.Count >= 2

    /// Register the theme state with Avalonia so that UI components can find it.
    do
        AvaloniaLocator.CurrentMutable.BindToSelf<IThemeSwitcher>(this)
        |> ignore

        /// Activate the current theme's style.
        styles.Add systemStyles.[styleIndex ()]

    interface IThemeSwitcher with
        member _.IsLight = theme = Theme.Light

        member this.Toggle() =
            this.Theme <- if theme = Theme.Light then Theme.Dark else Theme.Light
            let i = styleIndex ()
            styles.[0] <- systemStyles.[i]
            if havePluginStyle () then styles.[1] <- pluginStyles.[i]
            List.iter (fun s -> s this.Theme) subscribers

        member _.LoadPluginStyles plugin =
            pluginStyles <-
                [ plugin.LightStyle; plugin.DarkStyle ]
                |> toStyles

            let i = styleIndex ()
            if havePluginStyle () then styles.[1] <- pluginStyles.[i] else styles.Add pluginStyles.[i]

    /// The current theme selection.
    member this.Theme
        with get () = theme
        and private set (value) = theme <- value

    /// Subscribe to be notified when the theme is changed.
    member this.OnChanged(msg) =
        Cmd.ofSub (fun dispatch -> subscribers <- (msg >> dispatch) :: subscribers)
