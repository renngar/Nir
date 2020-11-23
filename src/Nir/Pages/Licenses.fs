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
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Nir.UI
open Nir.UI.Controls
open Nir.Web

// The current list of packages directly used by Nir include:
//
// Avalonia.Desktop - MIT
// FParsec - FParsec - FParsec License
// FSharp.Core - F# - MIT
// FSharp.Data - F# Data - Apache 2.0
// JaggerJo.Avalonia.FuncUI - Avalonia.FuncUI - MIT
// McMaster.NETCore.Plugins - .NET Core Plugins - Apache 2.0

type Model =
    { NoOp: bool }
    interface IPageModel with
        member this.HistoryStyle = NoHistory
        member this.Title = "Third-Party Open-Source Info"
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

let linkView dispatch (header: string) links license =
    let link url text =
        textBlock
            [ cls "link"
              onTapped (fun _ -> dispatch (OpenUrl url)) ]
            text

    TabItem.create [ TabItem.header header
                     TabItem.content
                         (border
                             [ Border.padding (4.0, 0.0) ]
                              (dockPanel [] [
                                  stackPanel [ dock Dock.Top ] (List.map (fun (url, text) -> link url text) links)
                                  About.license
                                      [ TextBox.margin (0.0, 10.0, 0.0, 0.0)
                                        dock Dock.Bottom ]
                                      license
                               ])) ]
    |> Helpers.generalize

let avaloniaLinksView (dispatch: Msg -> unit): IView =
    linkView
        dispatch
        "Avalonia"
        [ ("https://github.com/AvaloniaUI/Avalonia", "Main source repository")
          ("https://avaloniaui.net", "Documentation and information")
          ("https://gitter.im/AvaloniaUI/Avalonia", "Chat Room") ]
        "Avalonia.txt"


let avaloniaFuncUILinksView (dispatch: Msg -> unit): IView =
    linkView
        dispatch
        "Avalonia.FuncUI"
        [ ("https://github.com/AvaloniaCommunity/Avalonia.FuncUI", "Avalonia.FuncUI Repository")
          ("https://avaloniacommunity.github.io/Avalonia.FuncUI.Docs/", "Documentation")
          ("https://gitter.im/Avalonia-FuncUI", "Gitter") ]
        "Avalonia.FuncUI.txt"

let fparsecLinksView (dispatch: Msg -> unit): IView =
    linkView
        dispatch
        "FParsec"
        [ ("http://www.quanttec.com/fparsec/", "Documentation")
          ("https://github.com/stephan-tolksdorf/fparsec", "Source")
          ("http://www.quanttec.com/fparsec/license.html", "Full License with Links") ]
        "FParsec.txt"

let fsharpLinksView (dispatch: Msg -> unit): IView =
    linkView
        dispatch
        "F# Core"
        [ ("https://dotnet.microsoft.com/languages/fsharp", "F#")
          ("https://dotnet.microsoft.com/", ".NET Core")
          ("https://github.com/dotnet/fsharp", "Source") ]
        "FSharp.Core.txt"

let fsharpDataLinksView (dispatch: Msg -> unit): IView =
    linkView
        dispatch
        "F# Data"
        [ ("http://fsharp.github.io/FSharp.Data", "F# Data: Library for Data Access")
          ("http://github.com/fsharp/FSharp.Data", "Source Code on GitHub") ]
        "FSharp.Data.txt"

let hackLinksView (dispatch: Msg -> unit): IView =
    linkView
        dispatch
        "Hack Font"
        [ ("https://sourcefoundry.org/hack/", "Home Page")
          ("https://github.com/source-foundry/Hack", "Source") ]
        "Hack.txt"

let materialIconsLinksView (dispatch: Msg -> unit): IView =
    linkView
        dispatch
        "Material Icons"
        [ "https://material.io/resources/icons/?style=baseline", "Material Design Icons" ]
        "apache-2.0.txt"

let netCorePluginsLinksView (dispatch: Msg -> unit): IView =
    linkView
        dispatch
        ".NET Core Plugins"
        [ ("https://natemcmaster.com/blog/2018/07/25/netcore-plugins/", "Original Blog Post")
          ("https://github.com/natemcmaster/DotNetCorePlugins", "Source") ]
        "apache-2.0.txt"

let view (_: Model) (dispatch: Msg -> unit) =
    TabControl.create [ dock Dock.Top
                        TabControl.tabStripPlacement Dock.Left
                        TabControl.viewItems [ avaloniaLinksView dispatch
                                               avaloniaFuncUILinksView dispatch
                                               fparsecLinksView dispatch
                                               fsharpLinksView dispatch
                                               fsharpDataLinksView dispatch
                                               netCorePluginsLinksView dispatch
                                               hackLinksView dispatch
                                               materialIconsLinksView dispatch ] ]
    |> Helpers.generalize
