module Nir.Shell

open FSharp.Data.HttpStatusCodes

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.Components.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Threading

#if DEBUG
// Contrary to what IntelliSense may say, these are needed for setting up AttachDevTools in a debug build
open Avalonia // AttachDevTools
open Avalonia.Input // KeyGesture
#endif

open Nir.Pages
open Nir.NexusMods
open Nir.Utility.INI
open Nir.Utility.Path

// Model

type PageModel =
    | Start of Start.Model
    | ApiKey of ApiKey.Model
    | ErrorModel of Error.Model

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
    | StartMsg of Start.Msg
    | ApiKeyMsg of ApiKey.Msg
    | ErrorMsg of Error.Msg
    | ShellMsg of ShellMsg

let init window =
    let startPageModel, spCmd = Start.init window

    let key, ini =
        getProgramPath() +/ "Nir.ini"
        |> parseIniFile
        |> nexusApiKey
    { Ini = ini
      NexusApiKey = key
      Limits = RateLimits.initialLimits
      Page = Start startPageModel
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
                let m, cmd = Error.init "Error Contacting Nexus" msg Error.Buttons.RetryCancel
                { model with Page = ErrorModel m }, cmd
        | DisplayApiKeyPage ->
            let m, cmd = ApiKey.init
            { model with Page = ApiKey m }, cmd

    | StartMsg msg' ->
        match model.Page with
        | Start m ->
            let startPageModel, cmd = Start.update msg' m
            { model with Page = Start startPageModel }, Cmd.map StartMsg cmd
        | _ -> failwith "Mismatch between current page and message"

    | ApiKeyMsg msg' ->
        match model.Page with
        | ApiKey m ->
            match msg' with
            // Grab the results when the API Key page is done and write it to the .ini
            | ApiKey.Msg.Done { RateLimits = limits; Result = user } ->
                let newModel, cmd = Start.init model.Window
                let ini = setNexusApiKey model.Ini user.Key
                saveIni ini
                { model with
                      Ini = ini
                      NexusApiKey = user.Key
                      Limits = limits
                      Page = Start newModel }, cmd

            // Let it handle all the other messages
            | _ ->
                let apiKeyModel, cmd = ApiKey.update msg' m
                { model with Page = ApiKey apiKeyModel }, Cmd.map ApiKeyMsg cmd
        | _ -> failwith "Mismatch between current page and message"

    | ErrorMsg msg' ->
        match model.Page with
        | ErrorModel _ ->
            match msg' with
            | Error.Msg.Done button ->
                match button with
                // TODO: Once we fail to reach the Nexus site, we continue to get host errors after reconnecting.
                | "Retry" -> model, Cmd.ofMsg (ShellMsg VerifyApiKey)
                | _ ->
                    let newModel, cmd = Start.init model.Window
                    { model with Page = Start newModel }, cmd
        | _ -> failwith "Mismatch between current page and message"


// View

let view (model: Model) (dispatch: Msg -> unit) =
    match model.Page with
    | Start m -> Start.view m (StartMsg >> dispatch)
    | ApiKey m -> ApiKey.view m (ApiKeyMsg >> dispatch)
    | ErrorModel m -> Error.view m (ErrorMsg >> dispatch)

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
