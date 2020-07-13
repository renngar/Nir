module Nir.Shell

open Avalonia.Input
open Avalonia.Threading
open Elmish
open Avalonia
open Avalonia.FuncUI.Components.Hosts
open Avalonia.FuncUI.Elmish


// Model

type PageModel =
    | Start of StartPage.Model

type Model =
    { Page: PageModel }

let init window =
    let startPageModel, spCmd = StartPage.init window
    { Page = Start startPageModel },
    // TODO: Check this
    /// If your children controls don't emit any commands
    /// in the init function, you can just return Cmd.none
    /// otherwise, you can use a batch operation on all of them
    /// you can add more init commands as you need
    Cmd.batch [ spCmd ]


// Update

type Msg = StartPageMsg of StartPage.Msg

let update (msg: Msg) (model: Model): Model * Cmd<_> =
    match msg, model.Page with
    | StartPageMsg msg', Start m ->
        let startPageModel, cmd = StartPage.update msg' m
        { model with Page = Start startPageModel },
        Cmd.map StartPageMsg cmd


// View

let view (model: Model) (dispatch: Msg -> unit) =
    match model.Page with
    | Start m -> StartPage.view m (StartPageMsg >> dispatch)


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
