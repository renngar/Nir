module Nir.ApiKeyPage

open System.Diagnostics
open System.Runtime.InteropServices

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media

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

type Links = | NexusAccountPage

type Msg =
    | OpenUrl of Links
    | VerifyApiKey of string
    | AfterVerification of ApiResult<User>
    | Done of ApiSuccess<User> // Tell the "caller", the Shell, there are results

let update (msg: Msg) (model: Model): Model * Cmd<_> =
    match msg with
    | OpenUrl link ->
        let url =
            match link with
            | NexusAccountPage -> "https://www.nexusmods.com/users/myaccount?tab=api"
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            let start = sprintf "/c start %s" url
            Process.Start(ProcessStartInfo("cmd", start)) |> ignore
        else if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
            Process.Start("xdg-open", url) |> ignore
        else if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
            Process.Start("open", url) |> ignore

        model, Cmd.none

    | VerifyApiKey apiKey -> model, Cmd.OfAsync.perform usersValidate apiKey AfterVerification
    | AfterVerification(Ok { RateLimits = l; Result = user }) ->
        { RateLimit = Some l
          User = Some user
          ApiKey = user.Key }, Cmd.none
    | AfterVerification _ -> failwith "Not Implemented"
    | Done _ -> model, Cmd.none

// View

let view (model: Model) (dispatch: Msg -> unit) =
    let goodApiKey = model.RateLimit.IsSome && model.User.IsSome

    let (children: IView list) =
        [ TextBlock.create
            [ TextBlock.classes [ "title" ]
              TextBlock.text "Nexus API Key" ]
          TextBlock.create
              [ TextBlock.classes [ "subtitle" ]
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.text
                    ("Nir needs an API Key to communicate with Nexus.  You can get your Personal API "
                     + "Key from the API tab of the Nexus My Account Page.") ]
          Grid.create
              [ Grid.margin 10.0
                Grid.columnDefinitions "auto, *"
                Grid.children
                    [ Button.create
                        [ Grid.column 0
                          Button.isDefault (not goodApiKey)
                          Button.classes (if goodApiKey then [ "left" ] else [ "left"; "default" ])
                          Button.onClick (fun _ -> dispatch (OpenUrl NexusAccountPage))
                          Button.content "My Account Page" ]
                      TextBox.create
                          [ Grid.column 1
                            TextBox.textWrapping TextWrapping.Wrap
                            TextBox.watermark "Enter Your Personal API Key Manually"
                            TextBox.height 30.0
                            TextBox.verticalAlignment VerticalAlignment.Center
                            TextBox.acceptsReturn false
                            TextBox.acceptsTab false
                            // This is tacky, but Ctrl-Insert does not work with Avalonia
                            TextBox.tip (ToolTip.create [ ToolTip.content [ "Ctrl-V to paste" ] ])
                            TextBox.text model.ApiKey
                            TextBox.onTextChanged (VerifyApiKey >> dispatch) ] ] ] ]

    // If it validated successfully, display some account information
    let (extra: IView list) =
        match model.RateLimit, model.User with
        | Some limits, Some user ->
            [ TextBlock.create
                [ TextBlock.classes [ "subtitle" ]
                  TextBlock.textWrapping TextWrapping.Wrap
                  TextBlock.text ("Thanks " + user.Name + if user.IsPremium then " your premium!" else "!") ]

              Button.create
                  [ Button.isDefault true
                    Button.classes [ "left"; "default" ]
                    Button.content "Continue"
                    Button.onClick (fun _ ->
                        Done
                            ({ RateLimits = limits
                               Result = user })
                        |> dispatch) ] ]
        | _, _ -> []

    DockPanel.create
        [ DockPanel.children
            [ StackPanel.create
                [ StackPanel.margin 10.0
                  StackPanel.children (List.append children extra) ] ] ]
