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

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Nir.UI
open Nir.UI.Controls
open Nir.Web


type Model =
    { NoOp: bool }
    interface IPageModel with
        member this.HistoryStyle = NoHistory
        member this.Title = "Thank you for using Avalonia.FuncUI"
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

type Msg = OpenUrl of Links

type ExternalMsg = | NoOp

let init = { NoOp = false }, Cmd.none

let update (msg: Msg) (state: Model) =
    match msg with
    | OpenUrl link ->
        match link with
        | AvaloniaRepository -> "https://github.com/AvaloniaUI/Avalonia"
        | AvaloniaAwesome -> "https://github.com/AvaloniaCommunity/awesome-avalonia"
        | AvaloniaGitter -> "https://gitter.im/AvaloniaUI"
        | AvaloniaCommunity -> "https://github.com/AvaloniaCommunity"
        | FuncUIRepository -> "https://github.com/AvaloniaCommunity/Avalonia.FuncUI"
        | FuncUIGitter -> "https://gitter.im/Avalonia-FuncUI"
        | FuncUINetTemplates -> "https://github.com/AvaloniaCommunity/Avalonia.FuncUI.ProjectTemplates"
        | FuncUISamples -> "https://github.com/AvaloniaCommunity/Avalonia.FuncUI/tree/master/src/Examples"
        |> openUrl

        state, Cmd.none, NoOp

let linkView (dockingPosition: Dock) dispatch header links =
    let link url text =
        textBlock
            [ cls "link"
              onTapped (fun _ -> dispatch (OpenUrl url)) ]
            text

    stackPanel [ dock dockingPosition
                 horizontalAlignment
                     (if dockingPosition = Dock.Left then HorizontalAlignment.Left else HorizontalAlignment.Right) ] [
        yield textBlockCls "h1" header
        yield! Seq.map (fun (url, text) -> link url text) links
    ]

let avaloniaLinksView (dock: Dock) (dispatch: Msg -> unit): IView =
    linkView
        dock
        dispatch
        "Avalonia"
        [ (AvaloniaRepository, "Avalonia Repository")
          (AvaloniaAwesome, "Awesome Avalonia")
          (AvaloniaGitter, "Gitter")
          (AvaloniaCommunity, "Avalonia Community") ]

let avaloniaFuncUILinksView (dock: Dock) (dispatch: Msg -> unit): IView =
    linkView
        dock
        dispatch
        "Avalonia.FuncUI"
        [ (FuncUIRepository, "Avalonia.FuncUI Repository")
          (FuncUIGitter, "Gitter")
          (FuncUINetTemplates, ".Net Templates")
          (FuncUISamples, "Samples") ]

let view (_: Model) (dispatch: Msg -> unit) =
    dockPanel [ cls "about"
                horizontalAlignment HorizontalAlignment.Left
                verticalAlignment VerticalAlignment.Top ] [
        textBlock
            [ dock Dock.Top; cls "subtitle" ]
            ("Avalonia.FuncUI is a project that provides you with an Elmish DSL for Avalonia Controls\n"
             + "for you to use in an F# idiomatic way. We hope you like the project and spread the word :)\n"
             + "Questions ? Reach to us on Gitter, also check the links below")
        avaloniaLinksView Dock.Left dispatch
        avaloniaFuncUILinksView Dock.Right dispatch
    ]
