module Nir.Shell

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

open Nir.NexusMods
open Nir.Utility.INI
open Nir.Utility.Path

// Model

type PageModel =
    | Start of StartPage.Model
    | ApiKey of ApiKeyPage.Model

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
    | DisplayApiKeyPage

type Msg =
    | StartPageMsg of StartPage.Msg
    | ApiKeyPageMsg of ApiKeyPage.Msg
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
            model, Cmd.OfAsync.attempt usersValidate model.NexusApiKey (fun _ -> ShellMsg DisplayApiKeyPage)
        | DisplayApiKeyPage ->
            let m, cmd = ApiKeyPage.init
            { model with Page = ApiKey m }, cmd

    | StartPageMsg msg' ->
        match model.Page with
        | Start m ->
            let startPageModel, cmd = StartPage.update msg' m
            { model with Page = Start startPageModel }, Cmd.map StartPageMsg cmd
        | _ -> failwith "Mismatch between current page and message"

    | ApiKeyPageMsg msg' ->
        match model.Page with
        | ApiKey m ->
            match msg' with
            // Grab the results when the API Key page is done and write it to the .ini
            | ApiKeyPage.Msg.Done(limits, user) ->
                let newModel, cmd = StartPage.init model.Window
                let ini = setNexusApiKey model.Ini user.Key
                saveIni ini
                { model with
                      Ini = ini
                      NexusApiKey = user.Key
                      Limits = limits
                      Page = Start newModel }, cmd

            // Let it handle all the other messages
            | _ ->
                let apiKeyPageModel, cmd = ApiKeyPage.update msg' m
                { model with Page = ApiKey apiKeyPageModel }, Cmd.map ApiKeyPageMsg cmd
        | _ -> failwith "Mismatch between current page and message"


// View

let view (model: Model) (dispatch: Msg -> unit) =
    match model.Page with
    | Start m -> StartPage.view m (StartPageMsg >> dispatch)
    | ApiKey m -> ApiKeyPage.view m (ApiKeyPageMsg >> dispatch)

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
