// Nir error display page.
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
module Nir.Pages.Error

open Elmish
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Nir.UI
open Nir.UI.Controls

type ButtonType =
    | Retry
    | Cancel

type ButtonGroup = | RetryCancel

// Model

type Model =
    { Title: string
      Message: string
      Buttons: ButtonGroup }
    interface IPageModel with
        member this.HistoryStyle = Modal
        member this.Title = "Error"
        member this.Description = ""

let init title message buttons =
    { Title = title
      Message = message
      Buttons = buttons },
    Cmd.none

// Update
type ExternalMsg = ButtonType

type Msg = ButtonClicked of ButtonType

let update msg model: Model * Cmd<_> * ExternalMsg =
    match msg with
    | ButtonClicked button -> model, Cmd.none, button

// View

let private textBlock (cls: string list) contents =
    TextBlock.textBlock
        [ classes cls
          horizontalAlignment HorizontalAlignment.Center ]
        contents

let private button button' isDefault' (dispatch: Dispatch<Msg>) =
    textButton
        [ isDefault isDefault'
          onClick (fun _ -> dispatch (ButtonClicked button')) ]
        (match button' with
         | Retry -> "Retry"
         | Cancel -> "Cancel")

let private getButtons model dispatch: IView list =
    match model.Buttons with
    | RetryCancel ->
        [ button Retry true dispatch
          button Cancel false dispatch ]

let view (model: Model) (dispatch: Msg -> unit): IView =
    stackPanel [ cls "error"
                 horizontalAlignment HorizontalAlignment.Center ] [
        textBlock [ "h1"; "error" ] model.Title
        textBlock [ "h2" ] model.Message
        stackPanel
            [ cls "buttons"
              orientation Orientation.Horizontal
              horizontalAlignment HorizontalAlignment.Center ]
            (getButtons model dispatch)
    ]
