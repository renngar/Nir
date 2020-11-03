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
