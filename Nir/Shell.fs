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
    | DownloadChecker of DownloadChecker.Model

type Model =
    { Ini: Ini
      Nexus: Nexus

      // UI
      Page: PageModel
      Window: Window }

type ShellMsg =
    | VerifyApiKey
    | VerifiedApiKeyResult of ApiResult<User>
    | DisplayApiKeyPage

type Msg =
    | ShellMsg of ShellMsg
    | StartMsg of Start.Msg
    | ApiKeyMsg of ApiKey.Msg
    | ErrorMsg of Error.Msg
    | DownloadCheckerMsg of DownloadChecker.Msg

let init window =
    let startPageModel, spCmd = Start.init window

    let key, ini =
        getProgramPath() +/ "Nir.ini"
        |> parseIniFile
        |> nexusApiKey
    { Ini = ini
      Nexus =
          { ApiKey = key
            RateLimits = RateLimits.initialLimits }
      Page = Start startPageModel
      Window = window },
    Cmd.batch
        [ spCmd
          Cmd.ofMsg <| ShellMsg VerifyApiKey ]

type Update<'msg, 'model> = 'msg -> 'model -> ('model * Cmd<'msg>)

/// Update a specific page
let updatePage<'msg, 'model>
    (model: Model)
    (update: Update<'msg, 'model>)
    (msgType: 'msg -> Msg)
    (msg: 'msg)
    (modelType: 'model -> PageModel)
    (pageModel: 'model)
    =
    let newModel, cmd = update msg pageModel
    { model with Page = modelType newModel }, Cmd.map msgType cmd

// Update
let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let showPage pageModelType model (pageModel, cmd) = { model with Page = pageModelType pageModel }, cmd
    let showMainPage model = showPage DownloadChecker model <| DownloadChecker.init model.Window model.Nexus

    match msg, model.Page with
    | ShellMsg msg', _ ->
        match msg' with
        | VerifyApiKey ->
            model,
            Cmd.OfAsync.either usersValidate model.Nexus (ShellMsg << VerifiedApiKeyResult)
                (fun _ -> ShellMsg DisplayApiKeyPage)
        | VerifiedApiKeyResult x ->
            match x with
            | Ok _ -> showMainPage model
            | Error { StatusCode = Unauthorized; Message = _ } -> model, Cmd.ofMsg (ShellMsg DisplayApiKeyPage)
            | Error { StatusCode = _; Message = msg } ->
                Error.init "Error Contacting Nexus" msg Error.Buttons.RetryCancel |> showPage ErrorModel model
        | DisplayApiKeyPage -> ApiKey.init model.Nexus |> showPage ApiKey model

    // Grab the results when the API Key page is done and write it to the .ini
    | ApiKeyMsg(ApiKey.Msg.Done { Nexus = nexus; Result = user }), ApiKey _ ->
        let ini = setNexusApiKey model.Ini user.Key
        saveIni ini
        showMainPage
            { model with
                  Ini = ini
                  Nexus = nexus }

    | ErrorMsg(Error.Msg.Done "Retry"), ErrorModel _ -> model, Cmd.ofMsg (ShellMsg VerifyApiKey)
    | ErrorMsg(Error.Msg.Done _), ErrorModel _ ->
        model.Window.Close()
        model, Cmd.none

    | StartMsg msg', Start model' -> updatePage model Start.update StartMsg msg' Start model'
    | ApiKeyMsg msg', ApiKey model' -> updatePage model ApiKey.update ApiKeyMsg msg' ApiKey model'
    | DownloadCheckerMsg msg', DownloadChecker model' ->
        updatePage model DownloadChecker.update DownloadCheckerMsg msg' DownloadChecker model'

    // Should never happen
    | _, Start _
    | _, ApiKey _
    | _, DownloadChecker _
    | _, ErrorModel _ -> failwith "Mismatch between current page and message"

// View

let view (model: Model) (dispatch: Msg -> unit) =
    match model.Page with
    | Start m -> Start.view m (StartMsg >> dispatch)
    | ApiKey m -> ApiKey.view m (ApiKeyMsg >> dispatch)
    | ErrorModel m -> Error.view m (ErrorMsg >> dispatch)
    | DownloadChecker m -> DownloadChecker.view m (DownloadCheckerMsg >> dispatch)

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
