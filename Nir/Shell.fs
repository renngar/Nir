module Nir.Shell

open Elmish
open Avalonia
open Avalonia.FuncUI.Components.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Input
open Avalonia.Threading

open Nir.Core.NexusMods

// Model

type PageModel =
    | Start of StartPage.Model
    | ApiKey of ApiKeyPage.Model

type Model =
    { NexusApiKey: string
      Page: PageModel }

let init window =
    let startPageModel, spCmd = StartPage.init window
    let apkModel, apkCmd = ApiKeyPage.init
    let key = getNexusApiKey()
    // TODO: Check this
    /// If your children controls don't emit any commands
    /// in the init function, you can just return Cmd.none
    /// otherwise, you can use a batch operation on all of them
    /// you can add more init commands as you need
    { NexusApiKey = key
      Page =
          if key = "" then ApiKey apkModel else Start startPageModel },
    Cmd.batch [ spCmd; apkCmd ]


// Update

type Msg =
    | StartPageMsg of StartPage.Msg
    | ApiKeyPageMsg of ApiKeyPage.Msg

let update (msg: Msg) (model: Model): Model * Cmd<_> =
    match msg, model.Page with
    | StartPageMsg msg', Start m ->
        let startPageModel, cmd = StartPage.update msg' m
        { model with Page = Start startPageModel }, Cmd.map StartPageMsg cmd

    | ApiKeyPageMsg msg', ApiKey m ->
        let apiKeyPageModel, cmd = ApiKeyPage.update msg' m
        { model with Page = ApiKey apiKeyPageModel },
        Cmd.map ApiKeyPageMsg cmd
    | _, ApiKey _
    | _, Start _ -> failwith "mismatch between page and message type"



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

#if DEBUG
        this.AttachDevTools(KeyGesture(Key.F12))
#endif
        /// we use this function because sometimes we dispatch messages
        /// from another thread
        let syncDispatch (dispatch: Dispatch<'msg>): Dispatch<'msg> =
            match Dispatcher.UIThread.CheckAccess() with
            | true ->
                fun msg -> Dispatcher.UIThread.Post(fun () -> dispatch msg)
            | false -> dispatch

        Program.mkProgram init update view
        |> Program.withHost this
        |> Program.withSyncDispatch syncDispatch
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWith this
