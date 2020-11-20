// Nexus API key entry page.
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
module Nir.Pages.ApiKey

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media

open Nir.NexusApi
open Nir.UI
open Nir.UI.Controls
open Nir.Web

let NexusAccountPage =
    "https://www.nexusmods.com/users/myaccount?tab=api"

// Model

type Model =
    { Nexus: Nexus
      User: User option }
    interface IPageModel with
        member __.HistoryStyle = Normal
        member __.Title = "Nexus API Key"

        member __.Description =
            ("Nir needs an API Key to communicate with Nexus.  You can get your Personal API "
             + "Key from the API tab of the Nexus My Account Page.")

let init nexus = { Nexus = nexus; User = None }, Cmd.none

// Update

type ExternalMsg =
    | NoOp
    | Verified of Nexus * User

type Msg =
    | VerifyApiKey of string
    | AfterVerification of ApiResult<User>
    | Continue of User

let update msg model =
    match msg with
    | VerifyApiKey apiKey ->
        // TODO: Maybe switch ApiKey verification to Cmd.OfAsync.either splitting into two messages
        model, Cmd.OfAsync.perform model.Nexus.UsersValidate apiKey AfterVerification, NoOp
    | AfterVerification (Ok user) ->
        { Nexus = model.Nexus
          User = Some user },
        Cmd.none,
        NoOp
    | AfterVerification _ -> model, Cmd.none, NoOp
    | Continue user -> model, Cmd.none, Verified(model.Nexus, user)

// View

let view (model: Model) (dispatch: Msg -> unit): IView =
    let goodApiKey = model.User.IsSome

    let (children: IView list) =
        [ grid [ toColumnDefinitions "auto, *" ] [
            textButton
                [ column 0
                  isDefault (not goodApiKey)
                  classes (if goodApiKey then [] else [ "default" ])
                  onClick (fun _ -> openUrl NexusAccountPage) ]
                "My Account Page"
            textBox
                [ column 1
                  allowDrop true
                  onDragOver (fun e ->
                      e.DragEffects <-
                          if e.Data.Contains(DataFormats.Text) then
                              e.DragEffects &&& DragDropEffects.Copy
                          else
                              DragDropEffects.None)
                  onDrop (fun e ->
                      if e.Data.Contains(DataFormats.Text)
                      then e.Data.GetText() |> VerifyApiKey |> dispatch)
                  textWrapping TextWrapping.Wrap
                  TextBox.watermark "Enter Your Personal API Key Manually"
                  verticalAlignment VerticalAlignment.Center
                  acceptsReturn false
                  acceptsTab false
                  // This is tacky, but Ctrl-Insert does not work with Avalonia
                  toTip "Ctrl-V to paste"
                  onTextChanged (VerifyApiKey >> dispatch) ]
                model.Nexus.ApiKey
          ] ]

    // If it validated successfully, display some account information
    let extra: IView list =
        match model.User with
        | Some user ->
            [ textBlockCls
                "h2"
                  ("Thanks "
                   + user.Name
                   + if user.IsPremium then " you're premium!" else "!")

              textButton
                  [ isDefault true
                    onClick (fun _ -> dispatch (Continue user)) ]
                  "Continue" ]
        | None -> []

    stackPanelCls "apiKey" (List.append children extra)
