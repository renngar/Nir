namespace Nir

open Avalonia.Input
open Avalonia.Threading

/// This is the main module of your application
/// here you handle all of your child pages as well as their
/// messages and their updates, useful to update multiple parts
/// of your application, Please refer to the `view` function
/// to see how to handle different kinds of "*child*" controls
module Shell =
    open Elmish
    open Avalonia
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish

    type State =
        { AboutState: About.State
          StartPageState: StartPage.State }

    type Msg =
        | AboutMsg of About.Msg
        | StartPageMsg of StartPage.Msg

    let init window =
        let aboutState, bpCmd = About.init
        let startPageState, spCmd = StartPage.init window
        { AboutState = aboutState
          StartPageState = startPageState },
        /// If your children controls don't emit any commands
        /// in the init function, you can just return Cmd.none
        /// otherwise, you can use a batch operation on all of them
        /// you can add more init commands as you need
        Cmd.batch [ bpCmd; spCmd ]

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | AboutMsg msg' ->
            let aboutState, cmd =
                About.update msg' state.AboutState
            { state with AboutState = aboutState },
            /// map the message to the kind of message 
            /// your child control needs to handle
            Cmd.map AboutMsg cmd
        | StartPageMsg msg' ->
            let startPageState, cmd = StartPage.update msg' state.StartPageState
            { state with StartPageState = startPageState },
            Cmd.map StartPageMsg cmd

    let view (state: State) (dispatch: Msg -> unit) =
        StartPage.view state.StartPageState (StartPageMsg >> dispatch)

    /// This is the main window of your application
    /// you can do all sort of useful things here like setting heights and widths
    /// as well as attaching your dev tools that can be super useful when developing with
    /// Avalonia
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
                | true -> fun msg -> Dispatcher.UIThread.Post(fun () -> dispatch msg)
                | false -> dispatch

            Program.mkProgram init update view
            |> Program.withHost this
            |> Program.withSyncDispatch syncDispatch
#if DEBUG
            |> Program.withConsoleTrace
#endif
            |> Program.runWith this
