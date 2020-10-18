module Nir.Shell

open System
open FSharp.Data.HttpStatusCodes

open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Components.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Types
open Avalonia.Rendering
open Avalonia.Styling
open Avalonia.Threading

#if DEBUG
// Contrary to what IntelliSense may say, these are needed for setting up AttachDevTools in a debug build
open Avalonia.Input // KeyGesture
#endif

open Nir.Ini
open Nir.NexusApi
open Nir.Pages
open Nir.Parsing
open Nir.UI
open Nir.Utility.INI
open Nir.Utility.Path

type UnverifiedApiKey = string

type ShellMsg =
    | RetryView // Sent by setState if we are not ready to draw the next version of the view
    | VerifyApiKey of UnverifiedApiKey
    | VerifiedApiKeyResult of UnverifiedApiKey * ApiResult<User>
    | DisplayApiKeyPage

type Msg =
    | ShellMsg of ShellMsg
    | StartMsg of Start.Msg
    | ApiKeyMsg of ApiKey.Msg
    | ErrorMsg of Error.Msg
    | PluginMsg of Plugin.Msg

// Model

type ErrorModel = Msg * Error.Model

type PluginModel =
    { Plugin: IPlugin
      IniSection: SectionName
      Model: Plugin.Model }

type PageModel =
    | Start of Start.Model
    | ApiKey of ApiKey.Model
    | ErrorModel of ErrorModel
    | Plugin of PluginModel

type Model =
    { Ini: Ini
      Nexus: Nexus

      // UI
      Styles: Styles
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

let mutable private retryMsgInFlight = false

type ExternalMsg =
    | NoOp
    | StartExtMsg of Start.ExternalMsg
    | ApiKeyExtMsg of ApiKey.ExternalMsg
    | ErrorExtMsg of Error.ExternalMsg
    | PluginExtMsg of Plugin.ExternalMsg

let init (window, renderRoot, styles) =
    let startPageModel, _ = Start.init window

    let key, ini =
        parseIniFile (getProgramPath () +/ "Nir.ini")
        |> nexusApiKey

    { Ini = ini
      Nexus = Nexus(key)
      Styles = styles
      Window = window
      RenderRoot = renderRoot
      Page = Start startPageModel },
    Cmd.ofMsg (ShellMsg(VerifyApiKey key))

let private showPage pageModelType model (pageModel, cmd) =
    { model with
          Page = pageModelType pageModel },
    cmd

let private showMainPage model =
    let pageModel, cmd = Start.init model.Window
    showPage Start model (pageModel, Cmd.map StartMsg cmd)

// Update
let private updateShell msg model =
    match msg with
    | RetryView ->
        retryMsgInFlight <- false
        model, Cmd.none
    | VerifyApiKey key ->
        model,
        Cmd.OfAsync.either model.Nexus.UsersValidate key (fun result -> ShellMsg(VerifiedApiKeyResult(key, result))) (fun _ ->
            ShellMsg DisplayApiKeyPage)
    | VerifiedApiKeyResult (key, result) ->
        match result with
        | Ok _ -> showMainPage model
        | Error { StatusCode = Unauthorized; Message = _ } -> model, Cmd.ofMsg (ShellMsg DisplayApiKeyPage)
        | Error { StatusCode = _; Message = msg } ->
            let retryMsg = ShellMsg(VerifyApiKey key)

            let errorModel, cmd =
                Error.init "Error Contacting Nexus" msg Error.ButtonGroup.RetryCancel

            showPage ErrorModel model ((retryMsg, errorModel), cmd)
    | DisplayApiKeyPage -> ApiKey.init model.Nexus |> showPage ApiKey model
    |> fun (fst, snd) -> fst, snd, NoOp // Match the shape of the page-specific update functions

let private removeSpaces (s: string) = s.Replace(" ", "")

let private processPageMessage msg model =
    let updatePage update msg pageModel modelType msgType extMsgType =
        let newModel, cmd, extMsg = update msg pageModel
        { model with Page = modelType newModel }, Cmd.map msgType cmd, extMsgType extMsg

    match msg, model.Page with
    | ShellMsg shellMsg, _ -> updateShell shellMsg model
    | StartMsg startMsg, Start startModel -> updatePage Start.update startMsg startModel Start StartMsg StartExtMsg
    | ApiKeyMsg apiKeyMsg, ApiKey apiKeyModel ->
        updatePage ApiKey.update apiKeyMsg apiKeyModel ApiKey ApiKeyMsg ApiKeyExtMsg
    | ErrorMsg errorMsg, ErrorModel (retryMsg, errorModel) ->
        updatePage Error.update errorMsg errorModel (fun m -> ErrorModel(retryMsg, m)) ErrorMsg ErrorExtMsg
    | PluginMsg msg', Plugin pModel ->
        let newModel, cmd, extMsg = pModel.Plugin.Update(msg', pModel.Model)

        { model with
              Page = Plugin { pModel with Model = newModel } },
        Cmd.map PluginMsg cmd,
        PluginExtMsg extMsg

    // These should never happen, but are required for full pattern matching
    | StartMsg _, _
    | ApiKeyMsg _, _
    | ErrorMsg _, _
    | PluginMsg _, _ -> failwith "Mismatch between current page and message"

let private processExternalMessage model cmd externalMsg =
    match externalMsg with
    | NoOp
    | StartExtMsg Start.ExternalMsg.NoOp
    | ApiKeyExtMsg ApiKey.ExternalMsg.NoOp
    | PluginExtMsg Plugin.ExternalMsg.NoOp -> model, cmd

    | StartExtMsg (Start.ExternalMsg.LaunchPlugin plugin) ->
        // TODO Restructure this to give an error if a plugin cannot used as a section name
        let iniSection = create<SectionName> plugin.Name

        let { Properties = props }, _ = section iniSection model.Ini

        let pageModel, cmd =
            plugin.Init(model.Window, model.Nexus, props, model.ThrottleUpdates)

        model.Styles.Load plugin.LightStyle

        showPage
            Plugin
            model
            ({ Plugin = plugin
               IniSection = iniSection
               Model = pageModel },
             Cmd.map PluginMsg cmd)
    | ApiKeyExtMsg (ApiKey.ExternalMsg.Verified (nexus, user)) ->
        // Grab the results when the API Key page is done and write it to the .ini
        let ini = setNexusApiKey model.Ini user.Key
        saveIni ini
        showMainPage { model with Ini = ini; Nexus = nexus }
    | ErrorExtMsg Error.ExternalMsg.Retry ->
        match model.Page with
        | ErrorModel (retryMsg, _errorModel) -> model, Cmd.ofMsg retryMsg // processPageMessage already updated model
        | _ -> failwith "Should not happen"
    | ErrorExtMsg Error.ExternalMsg.Cancel ->
        model.Window.Close()
        model, Cmd.none
    | PluginExtMsg (Plugin.ExternalMsg.SaveProperties properties) ->
        match model.Page with
        | Plugin pModel ->
            let ini =
                (model.Ini, properties)
                ||> List.fold (fun ini p -> setIniProperty ini pModel.IniSection p.Property p.Value)

            saveIni ini

            { model with
                  Ini = ini
                  Page = Plugin pModel },
            Cmd.none
        | _ -> failwith "Only plugins should send a SaveProperties message"

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    processPageMessage msg model
    |||> processExternalMessage

// View

let view (model: Model) (dispatch: Msg -> unit): IView =
    match model.Page with
    | Start m -> Start.view m (StartMsg >> dispatch)
    | ApiKey m -> ApiKey.view m (ApiKeyMsg >> dispatch)
    | ErrorModel (_, m) -> Error.view m (ErrorMsg >> dispatch)
    | Plugin { Plugin = plugin; Model = m } -> plugin.View(m, PluginMsg >> dispatch)


let private stateRef = ref None

let private setState (host: IViewHost) retryMsg program (state: Model) dispatch =
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
        else
            () // Avoid Fantomas formatting problems
    else
        () // Avoid Fantomas formatting problems

/// Gives the deferred renderer a chance to draw the previous state before drawing the latest.
let private throttleUpdates host retryMsg program =
    program
    |> Program.withSetState (setState host retryMsg program)

// Main

type MainWindow(styles: Styles) as this =
    inherit HostWindow()

    // Copied from Avalonia.FuncUI.Elmish
    do
        base.Title <- "Nir"
        base.Width <- 800.0
        base.Height <- 600.0
        base.MinWidth <- 800.0
        base.MinHeight <- 600.0
        base.Padding <- Thickness(10.0, 5.0, 10.0, 10.0)

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
        |> Program.runWith (this, this.VisualRoot, styles)
