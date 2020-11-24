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

open System.Diagnostics
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media.Imaging
open Nir
open Nir.UI
open Nir.UI.Controls

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
    let tr =
        new StreamReader(Assets.Open("Licenses/" + filePath))

    tr.ReadToEnd()
    |> textBox [ cls "license"
                 dock Dock.Bottom
                 isReadOnly true
                 yield! attrs ]

let view (model: Model) (dispatch: Msg -> unit) =
    let icon = new Bitmap(Assets.Open("Icons/Nir.ico"))
    let img = Image()
    img.Source <- icon

    let nirVersion =
        FileVersionInfo.GetVersionInfo(Assembly.GetExecutingAssembly().Location).ProductVersion
        |> fun s -> Regex.Replace(s, @"\+.*", "")

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
                stackPanel [ orientation Orientation.Horizontal
                             verticalAlignment VerticalAlignment.Center
                             StackPanel.spacing 0.0 ] [
                    textBlock
                        [ cls "h1"
                          verticalAlignment VerticalAlignment.Bottom ]
                        ("Nir " + nirVersion)
                ]
            ]

        yield
            stackPanel [ StackPanel.width 600.0
                         dock Dock.Top ] [
                textBlock [] "Copyright © 2020 Renngar. All rights reserved.\n"
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

                textBlockCls
                    "subtitle"
                    ("under a variety of other licenses. You can read instructions on how to download and build "
                     + "for yourself")
                stackPanel [ orientation Orientation.Horizontal ] [
                    textBlock [] "the specific "
                    textBlock
                        [ cls "link"
                          onTapped (fun _ ->
                              Web.openUrl
                                  ("https://github.com/renngar/Nir/releases/tag/"
                                   + nirVersion)) ]
                        "source code used to create this copy"
                    textBlock [] "."
                ]
            ]
        if model.ShowNirLicense then
            yield
                license
                    [ TextBox.margin (0.0, 10.0, 0.0, 0.0)
                      dock Dock.Bottom ]
                    "gpl-3.0.txt"
    ]
