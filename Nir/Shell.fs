module Nir.Shell

open FSharp.Data.HttpStatusCodes

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.Components.Hosts
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Types
open Avalonia.Threading

#if DEBUG
// Contrary to what IntelliSense may say, these are needed for setting up AttachDevTools in a debug build
open Avalonia // AttachDevTools
open Avalonia.Input // KeyGesture
#endif

open Nir.NexusMods
open Nir.Utility.INI
open Nir.Utility.Path

// Model

type PageModel =
    | StartPage of StartPage.Model
    | ApiKeyPage of ApiKeyPage.Model
    | ErrorPage of ErrorPage.Model

type Model =
    {
      // Settings
      Ini: Ini

      // Web
      NexusApiKey: string
      Limits: RateLimits

      // UI
      Page: PageModel
      Window: Window }

type ShellMsg =
    | VerifyApiKey
    | VerifiedApiKeyResult of ApiResult<User>
    | DisplayApiKeyPage

type Msg =
    | StartPageMsg of StartPage.Msg
    | ApiKeyPageMsg of ApiKeyPage.Msg
    | ErrorPageMsg of ErrorPage.Msg
    | ShellMsg of ShellMsg

let init window =
    let startPageModel, spCmd = StartPage.init window

    let key, ini =
        getProgramPath() +/ "Nir.ini"
        |> parseIniFile
        |> nexusApiKey
    { Ini = ini
      NexusApiKey = key
      Limits = RateLimits.initialLimits
      Page = StartPage startPageModel
      Window = window },
    Cmd.batch
        [ spCmd
          Cmd.ofMsg <| ShellMsg VerifyApiKey ]


// Update
let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | ShellMsg msg' ->
        match msg' with
        | VerifyApiKey ->
            model,
            Cmd.OfAsync.either usersValidate model.NexusApiKey (ShellMsg << VerifiedApiKeyResult)
                (fun _ -> ShellMsg DisplayApiKeyPage)
        | VerifiedApiKeyResult x ->
            match x with
            | Ok _ -> model, Cmd.none
            | Error { StatusCode = Unauthorized; Message = _ } -> model, Cmd.ofMsg (ShellMsg DisplayApiKeyPage)
            | Error { StatusCode = _; Message = msg } ->
                let m, cmd = ErrorPage.init "Error Contacting Nexus" msg ErrorPage.Buttons.RetryCancel
                { model with Page = ErrorPage m }, cmd
        | DisplayApiKeyPage ->
            let m, cmd = ApiKeyPage.init
            { model with Page = ApiKeyPage m }, cmd

    | StartPageMsg msg' ->
        match model.Page with
        | StartPage m ->
            let startPageModel, cmd = StartPage.update msg' m
            { model with Page = StartPage startPageModel }, Cmd.map StartPageMsg cmd
        | _ -> failwith "Mismatch between current page and message"

    | ApiKeyPageMsg msg' ->
        match model.Page with
        | ApiKeyPage m ->
            match msg' with
            // Grab the results when the API Key page is done and write it to the .ini
            | ApiKeyPage.Msg.Done { RateLimits = limits; Result = user } ->
                let newModel, cmd = StartPage.init model.Window
                let ini = setNexusApiKey model.Ini user.Key
                saveIni ini
                { model with
                      Ini = ini
                      NexusApiKey = user.Key
                      Limits = limits
                      Page = StartPage newModel }, cmd

            // Let it handle all the other messages
            | _ ->
                let apiKeyPageModel, cmd = ApiKeyPage.update msg' m
                { model with Page = ApiKeyPage apiKeyPageModel }, Cmd.map ApiKeyPageMsg cmd
        | _ -> failwith "Mismatch between current page and message"

    | ErrorPageMsg msg' ->
        match model.Page with
        | ErrorPage _ ->
            match msg' with
            | ErrorPage.Msg.Done button ->
                match button with
                // TODO: Once we fail to reach the Nexus site, we continue to get host errors after reconnecting.
                | "Retry" -> model, Cmd.ofMsg (ShellMsg VerifyApiKey)
                | _ ->
                    let newModel, cmd = StartPage.init model.Window
                    { model with Page = StartPage newModel }, cmd
        | _ -> failwith "Mismatch between current page and message"


// View

let view (model: Model) (dispatch: Msg -> unit) =
    match model.Page with
    | StartPage m -> StartPage.view m (StartPageMsg >> dispatch)
    | ApiKeyPage m -> ApiKeyPage.view m (ApiKeyPageMsg >> dispatch)
    | ErrorPage m -> ErrorPage.view m (ErrorPageMsg >> dispatch)

// Main

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "Nir"
        base.Width <- 800.0
        base.Height <- 600.0
        base.MinWidth <- 800.0
        base.MinHeight <- 600.0

        //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        // TODO: Fix the About Avalonia on Mac
        NativeMenu.SetMenu(this, NativeMenu())

#if DEBUG
        this.AttachDevTools(KeyGesture(Key.F12))
#endif
        /// we use this function because sometimes we dispatch messages
        /// from another thread
        let syncDispatch (dispatch: Dispatch<'msg>): Dispatch<'msg> =
            match Dispatcher.UIThread.CheckAccess() with
            | true -> fun msg -> Dispatcher.UIThread.Post(fun () -> dispatch msg)
            | false -> dispatch

        Program.mkProgram init update view
        |> Program.withHost this
        |> Program.withSyncDispatch syncDispatch
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWith this
