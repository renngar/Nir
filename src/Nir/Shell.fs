module Nir.Shell

open System
open FSharp.Data.HttpStatusCodes

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.Components.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Types
open Avalonia.Rendering
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
    | Plugin of IPlugin * Plugin.Model

type Model =
    { Ini: Ini
      Nexus: Nexus

      // UI
      Page: PageModel
      RenderRoot: IRenderRoot
      Window: Window }

    /// Should progress updates be throttled or is the UI ready for more?
    member this.ThrottleUpdates() =
        match this.RenderRoot.Renderer with
        | :? IRenderLoopTask as task -> task.NeedsUpdate
        // The renderer may go away if the application is closed while background threads are updating.  Throttle
        // updates in this situation, because they won't be displayed anyway.
        | _ -> true

let mutable retryMsgInFlight = false

type ShellMsg =
    | RetryView // Sent by setState if we are not ready to draw the next version of the view
    | VerifyApiKey
    | VerifiedApiKeyResult of ApiResult<User>
    | DisplayApiKeyPage

type Msg =
    | ShellMsg of ShellMsg
    | StartMsg of Start.Msg
    | ApiKeyMsg of ApiKey.Msg
    | ErrorMsg of Error.Msg
    | PluginMsg of Plugin.Msg

type ExternalMsg =
    | NoOp
    | StartExtMsg of Start.ExternalMsg
    | ApiKeyExtMsg of ApiKey.ExternalMsg
    | ErrorExtMsg of Error.ExternalMsg

let init (window, renderRoot) =
    let startPageModel, _ = Start.init window

    let key, ini =
        getProgramPath() +/ "Nir.ini"
        |> parseIniFile
        |> nexusApiKey

    { Ini = ini
      Nexus =
          { ApiKey = key
            RateLimits = RateLimits.initialLimits }
      Window = window
      RenderRoot = renderRoot
      Page = Start startPageModel }, Cmd.ofMsg (ShellMsg VerifyApiKey)

let private showPage pageModelType model (pageModel, cmd) = { model with Page = pageModelType pageModel }, cmd

let private showMainPage model =
    let pageModel, cmd = Start.init model.Window
    showPage Start model (pageModel, Cmd.map StartMsg cmd)

// Update
let private updateShell msg model =
    match msg with
    | RetryView ->
        retryMsgInFlight <- false
        model, Cmd.none
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
    |> fun (fst, snd) -> fst, snd, NoOp // Match the shape of the page-specific update functions

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    let updatePage update msg pageModel modelType msgType extMsgType =
        let newModel, cmd, extMsg = update msg pageModel
        { model with Page = modelType newModel }, Cmd.map msgType cmd, extMsgType extMsg

    // Do the page updates
    let newModel, cmd, externalMsg =
        match msg, model.Page with
        | ShellMsg shellMsg, _ -> updateShell shellMsg model
        | StartMsg startMsg, Start startModel ->
            updatePage Start.update startMsg startModel Start StartMsg StartExtMsg
        | ApiKeyMsg apiKeyMsg, ApiKey apiKeyModel ->
            updatePage ApiKey.update apiKeyMsg apiKeyModel ApiKey ApiKeyMsg ApiKeyExtMsg
        | ErrorMsg errorMsg, ErrorModel errorModel ->
            updatePage Error.update errorMsg errorModel ErrorModel ErrorMsg ErrorExtMsg
        | PluginMsg msg', Plugin(plugin, pluginModel) ->
            let newModel, cmd = plugin.Update(msg', pluginModel)
            { model with Page = Plugin(plugin, newModel) }, Cmd.map PluginMsg cmd, NoOp

        // These should never happen, but are required for full pattern matching
        | StartMsg _, _
        | ApiKeyMsg _, _
        | ErrorMsg _, _
        | PluginMsg _, _ -> failwith "Mismatch between current page and message"

    // Then process the external messages they send
    match externalMsg with
    | NoOp
    | StartExtMsg Start.ExternalMsg.NoOp
    | ApiKeyExtMsg ApiKey.ExternalMsg.NoOp -> newModel, cmd

    | StartExtMsg(Start.ExternalMsg.LaunchPlugin plugin) ->
        let pageModel, cmd = plugin.Init(model.Window, model.Nexus, model.ThrottleUpdates)
        showPage Plugin model ((plugin, pageModel), Cmd.map PluginMsg cmd)
    | ApiKeyExtMsg(ApiKey.ExternalMsg.Verified { Nexus = nexus; Result = user }) ->
        // Grab the results when the API Key page is done and write it to the .ini
        let ini = setNexusApiKey model.Ini user.Key
        saveIni ini
        showMainPage
            { model with
                  Ini = ini
                  Nexus = nexus }
    | ErrorExtMsg Error.ExternalMsg.Retry -> model, Cmd.ofMsg (ShellMsg VerifyApiKey)
    | ErrorExtMsg Error.ExternalMsg.Cancel ->
        model.Window.Close()
        model, Cmd.none

// View

let view (model: Model) (dispatch: Msg -> unit): IView =
    match model.Page with
    | Start m -> Start.view m (StartMsg >> dispatch)
    | ApiKey m -> ApiKey.view m (ApiKeyMsg >> dispatch)
    | ErrorModel m -> Error.view m (ErrorMsg >> dispatch)
    | Plugin(plugin, m) -> plugin.View(m, PluginMsg >> dispatch)


// Give the deferred renderer a chance to draw the previous state before drawing the latest.
let throttleUpdates (host: IViewHost) (retryMsg: 'msg) (program: Program<'arg, Model, 'msg, #IView>) =
    let stateRef = ref None

    let setState (state: Model) dispatch =
        // create new view and update the host only if new model is not equal to a prev one
        let stateDiffers = (Some state).Equals(!stateRef) |> not

        if stateDiffers then
            let updated = state.ThrottleUpdates() |> not
            // Only build a new view if the screen has been updated with the previous one
            if updated then
                stateRef := Some state
                let view = ((Program.view program) state dispatch)
                host.Update(Some(view :> IView))
            elif retryMsgInFlight |> not then
                // Otherwise, dispatch a message after a while so that we can retry updating with the latest.
                DispatcherTimer.RunOnce(Action(fun () -> retryMsg |> dispatch), TimeSpan.FromMilliseconds 50.0)
                |> ignore
                retryMsgInFlight <- true

    program |> Program.withSetState setState

// Main

type MainWindow() as this =
    inherit HostWindow()

    // Copied from Avalonia.FuncUI.Elmish
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
        |> throttleUpdates this (ShellMsg RetryView)
        |> Program.withSyncDispatch syncDispatch
#if DEBUG
        |> Program.withTrace (fun msg _ -> printfn "New message: %A" msg)
#endif
        |> Program.runWith (this, this.VisualRoot)
