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
open Nir.NexusApi
open Nir.UI
open Nir.Utility.INI
open Nir.Utility.Path

// Model

type PageModel =
    | Start of Start.Model
    | ApiKey of ApiKey.Model
    | ErrorModel of Error.Model
    | PluginModel of Plugin.Model

type Model =
    { Ini: Ini
      Nexus: Nexus

      // UI
      Page: PageModel
      Window: Window
      Plugin: IPlugin option }

type ShellMsg =
    | VerifyApiKey
    | VerifiedApiKeyResult of ApiResult<User>
    | DisplayApiKeyPage

type Msg =
    | ShellMsg of ShellMsg
    | StartMsg of Start.Msg
    | ApiKeyMsg of ApiKey.Msg
    | ErrorMsg of Error.Msg
    | PluginMsg of Plugin.Msg

let init window =
    let startPageModel, _ = Start.init window

    let key, ini =
        getProgramPath() +/ "Nir.ini"
        |> parseIniFile
        |> nexusApiKey

    { Ini = ini
      Nexus =
          { ApiKey = key
            RateLimits = RateLimits.initialLimits }
      Page = Start startPageModel
      Window = window
      Plugin = None }, Cmd.ofMsg (ShellMsg VerifyApiKey)

type Update<'externalMsg, 'msg, 'model> = 'msg -> 'model -> ('model * Cmd<'msg> * 'externalMsg)

// Update
let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let showPage pageModelType model (pageModel, cmd) = { model with Page = pageModelType pageModel }, cmd

    let showMainPage model =
        let pageModel, cmd = Start.init model.Window
        showPage Start model (pageModel, Cmd.map StartMsg cmd)

    let mismatch() = failwith "Mismatch between current page and message"

    match msg with
    | ShellMsg msg' ->
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
                Error.init "Error Contacting Nexus" msg Error.ButtonGroup.RetryCancel |> showPage ErrorModel model
        | DisplayApiKeyPage -> ApiKey.init model.Nexus |> showPage ApiKey model


    | PluginMsg msg' ->
        match model.Page, model.Plugin with
        | PluginModel pluginModel, Some plugin ->
            let newModel, cmd = plugin.Update(msg', pluginModel)
            { model with Page = PluginModel newModel }, Cmd.map PluginMsg cmd
        | PluginModel _, None -> failwith "Mismatch between current page, message or plugin"
        | _ -> mismatch()

    // Page Messages
    | _ ->
        match msg, model.Page with
        | StartMsg startMsg, Start startModel ->
            let newStartModel, startCmd, startExternalMsg = Start.update startMsg startModel
            match startExternalMsg with
            | Start.ExternalMsg.NoOp -> { model with Page = Start newStartModel }, Cmd.map StartMsg startCmd
            | Start.ExternalMsg.LaunchPlugin plugin ->
                let pageModel, cmd = plugin.Init(model.Window, model.Nexus)
                showPage PluginModel { model with Plugin = Some plugin } (pageModel, Cmd.map PluginMsg cmd)

        | ApiKeyMsg apiKeyMsg, ApiKey apiKeyModel ->
            let newApiKeyModel, apiKeyCmd, apiKeyExternalMsg = ApiKey.update apiKeyMsg apiKeyModel
            match apiKeyExternalMsg with
            | ApiKey.ExternalMsg.NoOp -> { model with Page = ApiKey newApiKeyModel }, Cmd.map ApiKeyMsg apiKeyCmd
            // Grab the results when the API Key page is done and write it to the .ini
            | ApiKey.ExternalMsg.Verified { Nexus = nexus; Result = user } ->
                let ini = setNexusApiKey model.Ini user.Key
                saveIni ini
                showMainPage
                    { model with
                          Ini = ini
                          Nexus = nexus }

        | ErrorMsg errorMsg, ErrorModel errorModel ->
            let _, _, errorExternalMsg = Error.update errorMsg errorModel
            match errorExternalMsg with
            | Error.Retry -> model, Cmd.ofMsg (ShellMsg VerifyApiKey)
            | Error.Cancel ->
                model.Window.Close()
                model, Cmd.none

        // Should never happen
        | _, Start _
        | _, ApiKey _
        | _, ErrorModel _
        | _, PluginModel _ -> failwith "Mismatch between current page and message"

// View

let view (model: Model) (dispatch: Msg -> unit) =
    match model.Page, model.Plugin with
    | Start m, _ -> Start.view m (StartMsg >> dispatch)
    | ApiKey m, _ -> ApiKey.view m (ApiKeyMsg >> dispatch)
    | ErrorModel m, _ -> Error.view m (ErrorMsg >> dispatch)
    | PluginModel m, Some plugin -> plugin.View(m, PluginMsg >> dispatch)
    | PluginModel _, None -> failwith "Plugin unexpectedly missing"

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
        |> Program.withTrace (fun msg _ -> printfn "New message: %A" msg)
#endif
        |> Program.runWith this
