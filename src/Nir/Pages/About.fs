// About Nir page.
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

module Nir.Pages.About

open System
open System.IO
open global.Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media.Imaging
open Avalonia.Platform
open Nir.UI
open Nir.UI.Controls
open Nir.Utility.Path

type Model =
    { ShowNirLicense: bool }
    interface IPageModel with
        member this.HistoryStyle = NoHistory
        member this.Title = "About Nir"
        member this.Description = ""

type Msg =
    | ToggleGpl
    | ShowOpenSource

type ExternalMsg =
    | NoOp
    | ShowLicenses

let init = { ShowNirLicense = false }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | ToggleGpl ->
        { model with
              ShowNirLicense = not model.ShowNirLicense },
        Cmd.none,
        NoOp
    | ShowOpenSource -> model, Cmd.none, ShowLicenses

let license attrs filePath =
    File.ReadAllText
        (getProgramPath ()
         +/ @"Assets\Licenses"
         +/ filePath)
    |> textBox [ cls "license"
                 dock Dock.Bottom
                 isReadOnly true
                 yield! attrs ]

let view (model: Model) (dispatch: Msg -> unit) =
    let assets =
        AvaloniaLocator.Current.GetService<IAssetLoader>()

    let stream =
        assets.Open(Uri("avares://Nir/Assets/Icons/Nir.ico"))

    let icon = new Bitmap(stream)
    let img = Image()
    img.Source <- icon

    dockPanel [ cls "about"
                horizontalAlignment HorizontalAlignment.Center
                verticalAlignment VerticalAlignment.Top ] [
        yield
            stackPanel [ dock Dock.Top
                         orientation Orientation.Horizontal
                         StackPanel.width 600.0
                         StackPanel.spacing 10.0
                         StackPanel.margin (0.0, 0.0, 0.0, 10.0) ] [
                Image.create [ Image.source icon
                               Image.width 32.0
                               Image.height 32.0 ]
                textBlock
                    [ cls "h1"
                      TextBlock.verticalAlignment VerticalAlignment.Center ]
                    "Nir"
            ]

        yield
            stackPanel [ StackPanel.width 600.0
                         dock Dock.Top ] [
                yield textBlock [] "Copyright © 2020 Renngar. All rights reserved.\n"
                yield
                    stackPanel [ orientation Orientation.Horizontal ] [
                        textBlock [] "Nir is made available to you under the "
                        textBlock
                            [ cls "link"
                              onTapped (fun _ -> dispatch ToggleGpl) ]
                            "GNU Public License 3.0"
                        textBlock [] " (GPL) and includes "
                        textBlock
                            [ cls "link"
                              onTapped (fun _ -> dispatch ShowOpenSource) ]
                            "open source software"
                    ]
                yield!
                    List.map
                        (textBlockCls "subtitle")
                        [ "under a variety of other licenses. You can read instructions on how to download and build "
                          + "for yourself"
                          "the specific source code used to create this copy." ]
            ]
        if model.ShowNirLicense then
            yield
                license
                    [ TextBox.margin (0.0, 10.0, 0.0, 0.0)
                      dock Dock.Bottom ]
                    "gpl-3.0.txt"
    ]
