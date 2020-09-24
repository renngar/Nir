module Nir.Pages.ApiKey

open System.Diagnostics
open System.Runtime.InteropServices

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media

open Nir.NexusApi
open Nir.UI.Controls

// Model

type Model = { Nexus: Nexus; User: User option }

let init nexus = { Nexus = nexus; User = None }, Cmd.none

// Update

type Links = | NexusAccountPage

type ExternalMsg =
    | NoOp
    | Verified of ApiSuccess<User>

type Msg =
    | OpenUrl of Links
    | VerifyApiKey of ApiKey
    | AfterVerification of ApiResult<User>
    | Continue of ApiSuccess<User>

let update msg model =
    match msg with
    | OpenUrl link ->
        let url =
            match link with
            | NexusAccountPage -> "https://www.nexusmods.com/users/myaccount?tab=api"

        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            let start = sprintf "/c start %s" url

            Process.Start(ProcessStartInfo("cmd", start))
            |> ignore
        else if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
            Process.Start("xdg-open", url) |> ignore
        else if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
            Process.Start("open", url) |> ignore


        model, Cmd.none, NoOp

    | VerifyApiKey apiKey ->
        // TODO: Maybe switch ApiKey verification to Cmd.OfAsync.either splitting into two messages
        model, Cmd.OfAsync.perform usersValidate { model.Nexus with ApiKey = apiKey } AfterVerification, NoOp
    | AfterVerification (Ok { Nexus = nexus; Result = user }) ->
        { Nexus = { nexus with ApiKey = user.Key }
          User = Some user },
        Cmd.none,
        NoOp
    | AfterVerification _ -> model, Cmd.none, NoOp
    | Continue user -> model, Cmd.none, Verified user

// View

let view (model: Model) (dispatch: Msg -> unit): IView =
    let goodApiKey = model.User.IsSome

    let (children: IView list) =
        [ textBlock [ classes [ "h1" ] ] "Nexus API Key"
          textBlock
              [ classes [ "h2" ]
                TextBlock.textWrapping TextWrapping.Wrap ]
              ("Nir needs an API Key to communicate with Nexus.  You can get your Personal API "
               + "Key from the API tab of the Nexus My Account Page.")
          grid [ toColumnDefinitions "auto, *"
                 marginTopBottom 16.0 ] [
              textButton
                  [ column 0
                    marginRight 16.0
                    isDefault (not goodApiKey)
                    classes (if goodApiKey then [] else [ "default" ])
                    onClick (fun _ -> dispatch (OpenUrl NexusAccountPage)) ]
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
                    height 30.0
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
            [ textBlock
                [ classes [ "h2" ]
                  TextBlock.textWrapping TextWrapping.Wrap ]
                  ("Thanks "
                   + user.Name
                   + if user.IsPremium then " you're premium!" else "!")

              textButton
                  [ isDefault true
                    classes [ "default" ]
                    marginTopBottom 16.0
                    onClick (fun _ ->
                        Continue { Nexus = model.Nexus; Result = user }
                        |> dispatch) ]
                  "Continue" ]
        | None -> []

    stackPanel [ margin 10.0; spacing 4.0 ] (List.append children extra)
