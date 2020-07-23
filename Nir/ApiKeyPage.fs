module Nir.ApiKeyPage

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout

open NexusMods


// Model

type Model =
    { ApiKey: string
      RateLimit: RateLimits option
      User: User option }

let init =
    { ApiKey = ""
      RateLimit = None
      User = None }, Cmd.none

// Update

type Headers = Map<string, string>

type Msg =
    | VerifyApiKey of string
    | AfterVerification of ApiResult<User>
    | Done of ApiResult<User> // Tell the "caller", the Shell, there are results


let update (msg: Msg) (model: Model): Model * Cmd<_> =
    match msg with
    | VerifyApiKey apiKey -> model, Cmd.OfAsync.perform NexusMods.usersValidate apiKey AfterVerification

    | AfterVerification(l, u) ->
        { model with
              RateLimit = Some l
              User = Some u }, Cmd.none

    | Done _ -> model, Cmd.none

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
        match model.RateLimit, model.User with
        // If it validated successfully, display some account information
        | Some limits, Some user ->
            [ labelledText "User" (user.UserId.ToString())
              labelledText "Name" user.Name
              CheckBox.create
                  [ CheckBox.isEnabled false
                    CheckBox.content "Premium"
                    CheckBox.isChecked user.IsPremium ]
              Button.create
                  [ Button.content "Continue"
                    Button.onClick (fun _ -> Done(limits, user) |> dispatch) ] ]
        // If the Nexus account has not been validated, prompt for an API key
        | _, _ ->
            [ TextBox.create
                [ TextBox.watermark "Enter API Key Manually"
                  TextBox.text model.ApiKey
                  TextBox.onTextChanged (VerifyApiKey >> dispatch) ] ]


    StackPanel.create [ StackPanel.children content ]
// button "Connect to Nexus"
// button "Enter API Key Manually"
// button "Disconnect from Nexus"

let view (m: Model) (dispatch: Msg -> unit) = DockPanel.create [ DockPanel.children [ theView m dispatch ] ]
