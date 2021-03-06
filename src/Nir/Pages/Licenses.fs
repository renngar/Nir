// Open-source licensing page.
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

module Nir.Pages.Licenses

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Nir.UI
open Nir.UI.Controls
open Nir.Web

type Model =
    { NoOp: bool }
    interface IPageModel with
        member this.HistoryStyle = NoHistory
        member this.Title = "Third-Party Open Source Info"
        member this.Description = ""

type Links =
    | AvaloniaRepository
    | AvaloniaAwesome
    | AvaloniaGitter
    | AvaloniaCommunity
    | FuncUIRepository
    | FuncUIGitter
    | FuncUINetTemplates
    | FuncUISamples

type Msg = OpenUrl of string

type ExternalMsg = | NoOp

let init = { NoOp = false }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | OpenUrl link ->
        openUrl link
        model, Cmd.none, NoOp

let licenseView dispatch (header: string) links license =
    let link url text =
        textBlock
            [ cls "link"
              onTapped (fun _ -> dispatch (OpenUrl url)) ]
            text

    let linkView =
        stackPanel [ dock Dock.Top ] (List.map (fun (url, text) -> link url text) links)

    tabItem
        []
        header
        (dockPanel [] [
            linkView
            About.license [ dock Dock.Bottom ] license
         ])

let avaloniaLinksView (dispatch: Msg -> unit): IView =
    licenseView
        dispatch
        "Avalonia"
        [ ("https://github.com/AvaloniaUI/Avalonia", "Main source repository")
          ("https://avaloniaui.net", "Documentation and information")
          ("https://gitter.im/AvaloniaUI/Avalonia", "Chat Room") ]
        "Avalonia.txt"


let avaloniaFuncUILinksView (dispatch: Msg -> unit): IView =
    licenseView
        dispatch
        "Avalonia.FuncUI"
        [ ("https://github.com/AvaloniaCommunity/Avalonia.FuncUI", "Avalonia.FuncUI Repository")
          ("https://avaloniacommunity.github.io/Avalonia.FuncUI.Docs/", "Documentation")
          ("https://gitter.im/Avalonia-FuncUI", "Gitter") ]
        "Avalonia.FuncUI.txt"

let fparsecLinksView (dispatch: Msg -> unit): IView =
    licenseView
        dispatch
        "FParsec"
        [ ("http://www.quanttec.com/fparsec/", "Documentation")
          ("https://github.com/stephan-tolksdorf/fparsec", "Source")
          ("http://www.quanttec.com/fparsec/license.html", "Full License with Links") ]
        "FParsec.txt"

let fsharpLinksView (dispatch: Msg -> unit): IView =
    licenseView
        dispatch
        "F# Core"
        [ ("https://dotnet.microsoft.com/languages/fsharp", "F#")
          ("https://dotnet.microsoft.com/", ".NET Core")
          ("https://github.com/dotnet/fsharp", "Source") ]
        "FSharp.Core.txt"

let fsharpDataLinksView (dispatch: Msg -> unit): IView =
    licenseView
        dispatch
        "F# Data"
        [ ("http://fsharp.github.io/FSharp.Data", "F# Data: Library for Data Access")
          ("http://github.com/fsharp/FSharp.Data", "Source Code on GitHub") ]
        "FSharp.Data.txt"

let hackLinksView (dispatch: Msg -> unit): IView =
    licenseView
        dispatch
        "Hack Font"
        [ ("https://sourcefoundry.org/hack/", "Home Page")
          ("https://github.com/source-foundry/Hack", "Source") ]
        "Hack.txt"

let materialIconsLinksView (dispatch: Msg -> unit): IView =
    licenseView
        dispatch
        "Material Icons"
        [ "https://material.io/resources/icons/?style=outline", "Material Design Icons" ]
        "apache-2.0.txt"

let netCorePluginsLinksView (dispatch: Msg -> unit): IView =
    licenseView
        dispatch
        ".NET Core Plugins"
        [ ("https://natemcmaster.com/blog/2018/07/25/netcore-plugins/", "Original Blog Post")
          ("https://github.com/natemcmaster/DotNetCorePlugins", "Source") ]
        "apache-2.0.txt"

let view (_: Model) (dispatch: Msg -> unit) =
    tabControlCls
        "licenses"
        [ avaloniaLinksView dispatch
          avaloniaFuncUILinksView dispatch
          fparsecLinksView dispatch
          fsharpLinksView dispatch
          fsharpDataLinksView dispatch
          netCorePluginsLinksView dispatch
          hackLinksView dispatch
          materialIconsLinksView dispatch ]
