module Nir.ApiKeyPage

open Avalonia.FuncUI.Types
open FSharp.Data

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open Nir.Controls
open Nir.Dialogs
// Model

type Validate = NexusMods.ValidateProvider.Validate

type Model =
    { ApiKey: string
      Validated: Validate option }

let init =
    { ApiKey = ""
      Validated = None }, Cmd.none

// Update

type Headers = Map<string, string>

type Msg =
    | VerifyApiKey of string
    | AfterVerification of Headers * Validate


let update (msg: Msg) (model: Model): Model * Cmd<_> =
    match msg with
    | VerifyApiKey apiKey -> model, Cmd.OfAsync.perform NexusMods.usersValidate apiKey AfterVerification

    | AfterVerification(headers, v) -> { model with Validated = Some v }, Cmd.none

// View

let button (text: string) = Button.create [ Button.content text ]

let labelledText label content =
    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.children
              [ TextBlock.create [ TextBlock.text (label + ": ") ]
                TextBlock.create [ TextBlock.text content ] ] ]

let private theView (model: Model) (dispatch: Msg -> unit) =
    let (content: IView list) =
        match model.Validated with
        // If the Nexus account has not been validated, prompt for an API key
        | None ->
            [ TextBox.create
                [ TextBox.watermark "Enter API Key Manually"
                  TextBox.text model.ApiKey
                  TextBox.onTextChanged (VerifyApiKey >> dispatch) ] ]

        // If it validated successfully, display some account information
        | Some v ->
            [ labelledText "User" (v.UserId.ToString())
              labelledText "Name" v.Name
              CheckBox.create
                  [ CheckBox.isEnabled false
                    CheckBox.content "Premium"
                    CheckBox.isChecked v.IsPremium ] ]

    StackPanel.create [ StackPanel.children content ]
// button "Connect to Nexus"
// button "Enter API Key Manually"
// button "Disconnect from Nexus"

let view (m: Model) (dispatch: Msg -> unit) = DockPanel.create [ DockPanel.children [ theView m dispatch ] ]
