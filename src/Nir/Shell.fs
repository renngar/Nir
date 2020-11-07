module Nir.Shell

open System
open FSharp.Data.HttpStatusCodes

open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.Components.Hosts
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Types
open Avalonia.Interactivity
open Avalonia.Rendering
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
open Nir.UI.Controls
open Nir.UI.Material
open Nir.Utility.INI
open Nir.Utility.Path

type UnverifiedApiKey = string

type ShellMsg =
    | ThemeChanged of Theme
    | RetryView // Sent by setState if we are not ready to draw the next version of the view
    | VerifyApiKey of UnverifiedApiKey
    | RetryApiKey of UnverifiedApiKey
    | VerifiedApiKeyResult of UnverifiedApiKey * ApiResult<User>
    | ShowApiKeyPage
    | ShowAboutPage
    | BackPage
    | ForwardPage

type Msg =
    | ShellMsg of ShellMsg
    | StartMsg of Start.Msg
    | AboutMsg of About.Msg
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
    | About of About.Model
    | ApiKey of ApiKey.Model
    | ErrorModel of ErrorModel
    | Plugin of PluginModel

type Page =
    { Type: Type
      PageModel: PageModel }

    member this.Plugin =
        match this.PageModel with
        | Plugin p -> p
        | _ -> failwith "Not a plugin"

    member this.RawPageModel =
        match this.PageModel with
        | Start m -> m :> IPluginModel
        | About m -> m :> IPluginModel
        | ApiKey m -> m :> IPluginModel
        | ErrorModel (_, m) -> m :> IPluginModel
        | Plugin { Model = (Plugin.Model m) } -> m

    member this.HistoryStyleIs style =
        let pm = this.RawPageModel

        // Look for either an IPageModel with a matching HistoryStyle or if it is not an IPageModel, it is a plugin,
        // which has an implicit `Normal` style that we check for.
        (pm :? IPageModel)
        && (pm :?> IPageModel).HistoryStyle = style
        || style = Normal

    member this.AddToHistory = this.HistoryStyleIs Normal

    member this.HideHistoryButtons = this.HistoryStyleIs Modal

type Pages = Page list

type Model =
    { Ini: Ini
      Nexus: Nexus

      // UI
      BackPages: Pages
      CurrentPage: Page
      ForwardPages: Pages
      OtherPages: Pages

      RenderRoot: IRenderRoot
      Window: Window }

    /// Should progress updates be throttled or is the UI ready for more?
    member this.ThrottleUpdates() =
        match this.RenderRoot.Renderer with
        | :? IRenderLoopTask as task -> task.NeedsUpdate
        // The renderer may go away if the application is closed while background threads are updating.  Throttle
        // updates in this situation, because they won't be displayed anyway.
        | _ -> true

    /// Get the model for the current page
    member this.RawPageModel = this.CurrentPage.RawPageModel

let mutable private retryMsgInFlight = false

type ExternalMsg =
    | NoOp
    | StartExtMsg of Start.ExternalMsg
    | AboutExtMsg of About.ExternalMsg
    | ApiKeyExtMsg of ApiKey.ExternalMsg
    | ErrorExtMsg of Error.ExternalMsg
    | PluginExtMsg of Plugin.ExternalMsg

let init (window, renderRoot, ini) =
    let startPageModel, _ = Start.init window

    let key = nexusApiKey ini

    { Ini = ini
      Nexus = Nexus(nexusApiKey ini)
      Window = window
      RenderRoot = renderRoot
      BackPages = []
      CurrentPage =
          { Type = typeof<Start.Model>
            PageModel = Start startPageModel }
      ForwardPages = []
      OtherPages = [] },
    Cmd.ofMsg (ShellMsg(VerifyApiKey key))

/// Try to find a page with the given `modelType` in the page lists.
///
/// NOTE: This does not check `CurrentPage`.
let private tryFindPage (model: Model) (modelType: Type): Page option =
    /// If `page` is `None`, try finding a page of type `modelType` in `pages`.
    let (<|>) (page: Page option) (pages: Page list): Page option =
        page
        |> Option.orElseWith (fun () -> List.tryFind (fun (p: Page) -> p.Type = modelType) pages)

    None
    <|> model.BackPages
    <|> model.ForwardPages
    <|> model.OtherPages

/// Display the requested page, either calling its `init` function or pulling its state from the history lists, if it
/// has been displayed before.
let private showPage (modelType: Type)
                     (unionType: 'Model -> PageModel)
                     (cmdType: 'Msg -> Msg)
                     (model: Model)
                     (init: unit -> 'Model * Cmd<'Msg>)
                     : Model * Cmd<Msg> =
    let matchesType (p: Page) = p.Type = modelType

    let page, cmd =
        tryFindPage model modelType
        |> function
        | Some page -> page, Cmd.map cmdType Cmd.none
        | None ->
            let m, c = init ()

            { Type = typeof<'Model>
              PageModel = unionType m },
            Cmd.map cmdType c

    let filter lst = List.filter (not << matchesType) lst

    // Remove the page from any history list where it may exist, we already retrieved it above.
    let newModel =
        { model with
              BackPages = filter model.BackPages
              ForwardPages = filter model.ForwardPages
              OtherPages = filter model.OtherPages }

    if modelType = newModel.CurrentPage.Type then
        { newModel with CurrentPage = page }, cmd
    else
        { newModel with
              BackPages =
                  if model.CurrentPage.AddToHistory
                  then newModel.CurrentPage :: newModel.BackPages
                  else newModel.BackPages
              CurrentPage = page
              // If the page gets added to history, add ForwardPages to OtherPages and clear ForwardPages, this retains
              // the state of any forward pages while maintaining a linear forward-and-backward navigation model.
              ForwardPages = if page.AddToHistory then [] else newModel.ForwardPages
              OtherPages =
                  if page.AddToHistory
                  then List.append newModel.OtherPages newModel.ForwardPages
                  else newModel.OtherPages },
        cmd

let private showStartPage model =
    fun () -> Start.init model.Window
    |> showPage typeof<Start.Model> Start StartMsg model

// Update
let private updateShell (msg: ShellMsg) (model: Model): Model * Cmd<Msg> * ExternalMsg =
    match msg with
    | RetryView ->
        retryMsgInFlight <- false
        model, Cmd.none
    | VerifyApiKey key ->
        model,
        Cmd.OfAsync.either model.Nexus.UsersValidate key (fun result -> ShellMsg(VerifiedApiKeyResult(key, result))) (fun _ ->
            ShellMsg ShowApiKeyPage)
    | RetryApiKey key -> showStartPage model |> fst, Cmd.ofMsg (ShellMsg(VerifyApiKey key))
    | VerifiedApiKeyResult (key, result) ->
        match result with
        | Ok _ -> showStartPage model
        | Error { StatusCode = Unauthorized; Message = _ } -> model, Cmd.ofMsg (ShellMsg ShowApiKeyPage)
        | Error { StatusCode = _; Message = msg } ->
            fun () ->
                let retryMsg = ShellMsg(RetryApiKey key)

                let errorModel, cmd =
                    Error.init "Error Contacting Nexus" msg Error.ButtonGroup.RetryCancel

                ((retryMsg, errorModel), cmd)
            |> showPage typeof<ErrorModel> ErrorModel ErrorMsg model
    | ShowApiKeyPage ->
        fun () -> ApiKey.init model.Nexus
        |> showPage typeof<ApiKey.Model> ApiKey ApiKeyMsg model
    | ShowAboutPage ->
        fun () -> About.init
        |> showPage typeof<About.Model> About AboutMsg model
    | BackPage ->
        { model with
              ForwardPages =
                  if model.CurrentPage.AddToHistory then model.CurrentPage :: model.ForwardPages else model.ForwardPages
              CurrentPage = model.BackPages.Head
              BackPages = model.BackPages.Tail },
        Cmd.none
    | ForwardPage ->
        { model with
              ForwardPages = model.ForwardPages.Tail
              CurrentPage = model.ForwardPages.Head
              BackPages = model.CurrentPage :: model.BackPages },
        Cmd.none
    | ThemeChanged theme ->
        setTheme model.Ini theme
        |> saveIni
        |> fun ini -> { model with Ini = ini }, Cmd.none
    |> fun (fst, snd) -> fst, snd, NoOp // Match the shape of the page-specific update functions

let private removeSpaces (s: string) = s.Replace(" ", "")

/// Holds the command that results from updating the page.
let mutable private cmd = Cmd.none
/// Holds the external message that results from updating the page.
let mutable private extMsg = ExternalMsg.NoOp

/// Applies `updater` to any page of `modelType` regardless of whether it is the `CurrentPage` or is in one of the
/// page lists.
///
/// The `updater` is expected to set the mutable `cmd` and `extMsg` variables when a match is found.  They are
/// eventually returned after all the lists are searched.  This avoids a bunch of conditional logic to short circuit
/// searching all the lists.
let private updatePageLists (model: Model) (modelType: Type) (updater: Page -> Page): Model * Cmd<Msg> * ExternalMsg =
    let updateMatchingType (page: Page): Page =
        if page.Type = modelType then updater page else page

    { model with
          BackPages = List.map updateMatchingType model.BackPages
          CurrentPage = updateMatchingType model.CurrentPage
          ForwardPages = List.map updateMatchingType model.ForwardPages
          OtherPages = List.map updateMatchingType model.OtherPages },
    cmd,
    extMsg

/// Updates a specific type of page regardless of whether it is the `CurrentPage` or is in one of the page lists.
///
/// The difference between this and `updatePageLists` is that `updatePageLists` is a lower-level function which is
/// used by both this function and the update logic for plugin pages.
///
/// The `updateModel` function gets the `PageModel` of the page and its newly updated model in a tuple.  For simple
/// pages, the new model may be extracted using `snd` and wrapped with a `PageModel` discriminator.
let updatePage (model: Model)
               (modelType: Type)
               (update: 'PageMsg -> 'RawPageModel -> 'RawPageModel * Cmd<'PageMsg> * 'PageExtMsg)
               (msg: 'PageMsg)
               (updateModel: PageModel * 'RawPageModel -> PageModel)
               (msgType: 'PageMsg -> Msg)
               (extMsgType: 'PageExtMsg -> ExternalMsg)
               : Model * Cmd<Msg> * ExternalMsg =
    (fun (page: Page) ->
        let newModel, c, eMsg =
            update msg (page.RawPageModel :?> 'RawPageModel)

        cmd <- Cmd.map msgType c
        extMsg <- extMsgType eMsg

        { page with
              PageModel = updateModel (page.PageModel, newModel) })
    |> updatePageLists model modelType

/// Processes a page message regardless of whether it is the `CurrentPage` or is in one of the page history lists.
let private processPageMessage msg model =
    match msg with
    | ShellMsg shellMsg -> updateShell shellMsg model
    | StartMsg startMsg ->
        updatePage model typeof<Start.Model> Start.update startMsg (snd >> Start) StartMsg StartExtMsg
    | AboutMsg aboutMsg ->
        updatePage model typeof<About.Model> About.update aboutMsg (snd >> About) AboutMsg AboutExtMsg
    | ApiKeyMsg apiKeyMsg ->
        updatePage model typeof<ApiKey.Model> ApiKey.update apiKeyMsg (snd >> ApiKey) ApiKeyMsg ApiKeyExtMsg
    | ErrorMsg errorMsg ->
        /// Throws away the old model, replacing it with the new one and retaining the existing retry message.
        let updateModel: PageModel * Error.Model -> PageModel =
            function
            | ErrorModel (retryMsg, _oldModel), newModel -> ErrorModel(retryMsg, newModel)
            | _ -> failwith "Mismatch between page model and message" // should never happen

        updatePage model typeof<ErrorModel> Error.update errorMsg updateModel ErrorMsg ErrorExtMsg
    | PluginMsg pluginMsg ->
        // This performs the same basic logic that `updatePage` does but uses the plugin update mechanism rather than a
        // standard Elmish update function.
        updatePageLists model typeof<PluginModel> (fun page ->
            let p = page.Plugin
            let newModel, c, eMsg = p.Plugin.Update(pluginMsg, p.Model)

            cmd <- Cmd.map PluginMsg c
            extMsg <- PluginExtMsg eMsg

            { page with
                  PageModel = Plugin { p with Model = newModel } })

let private processExternalMessage model cmd externalMsg =
    match externalMsg with
    | NoOp
    | StartExtMsg Start.ExternalMsg.NoOp
    | AboutExtMsg About.ExternalMsg.NoOp
    | ApiKeyExtMsg ApiKey.ExternalMsg.NoOp
    | PluginExtMsg Plugin.ExternalMsg.NoOp -> model, cmd

    | StartExtMsg (Start.ExternalMsg.LaunchPlugin plugin) ->
        showPage typeof<PluginModel> Plugin PluginMsg model (fun () ->
            // TODO Restructure this to give an error if a plugin cannot used as a section name
            let iniSection =
                create<SectionName> ("Plugin: " + plugin.Name)

            let { Properties = props } = section iniSection model.Ini

            let pageModel, cmd =
                plugin.Init(model.Window, model.Nexus, props, model.ThrottleUpdates)

            AvaloniaLocator.Current.GetService<IThemeSwitcher>().LoadPluginStyles plugin

            ({ Plugin = plugin
               IniSection = iniSection
               Model = pageModel },
             cmd))
    | ApiKeyExtMsg (ApiKey.ExternalMsg.Verified (nexus, user)) ->
        // Grab the results when the API Key page is done and write it to the .ini
        setNexusApiKey model.Ini user.Key
        |> saveIni
        // Show the start page
        |> fun ini -> showStartPage { model with Ini = ini; Nexus = nexus }
        // Now that we have an ApiKey it is safe to find and start plugins.
        |> fun (m, c) ->
            m,
            Cmd.batch [ c
                        Cmd.ofMsg (StartMsg Start.GetPlugins) ]
    | ErrorExtMsg Error.ExternalMsg.Retry ->
        match model.CurrentPage.PageModel with
        | ErrorModel (retryMsg, _errorModel) -> model, Cmd.ofMsg retryMsg // processPageMessage already updated model
        | _ -> failwith "Should not happen"
    | ErrorExtMsg Error.ExternalMsg.Cancel ->
        model.Window.Close()
        model, Cmd.none
    | PluginExtMsg (Plugin.ExternalMsg.SaveProperties properties) ->
        let page =
            tryFindPage model typeof<PluginModel>
            |> Option.defaultValue model.CurrentPage

        let section = page.Plugin.IniSection

        { model with
              Ini =
                  (model.Ini, properties)
                  ||> List.fold (fun ini p -> setIniProperty ini section p.Property p.Value)
                  |> saveIni },
        Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    processPageMessage msg model
    |||> processExternalMessage

// View
let private pageHeader (model: Model) (dispatch: Dispatch<Msg>) =
    let theme =
        AvaloniaLocator.Current.GetService<IThemeSwitcher>()

    let pageModel = model.RawPageModel
    let titleBlock = textBlockCls "h1" pageModel.Title

    let descriptionBlock =
        if pageModel.Description <> "" then
            [ pageModel.Description
              |> textBlock [ cls "description" ] ]
        else
            []

    let buttonsAndMenu =
        stackPanelCls
            "headerButtons"
            [ let materialButton (clickHandler: RoutedEventArgs -> unit) (icon: string) (enabled: bool): IView =
                textButton
                    [ cls "material"
                      onClick clickHandler
                      isEnabled enabled ]
                    icon

              // Check for more than one page without walking the list to get its length.
              if not model.BackPages.IsEmpty then
                  yield
                      materialButton (fun _ -> dispatch (ShellMsg BackPage)) Icons.arrowBack
                          (not model.CurrentPage.HideHistoryButtons)

              if not model.BackPages.IsEmpty
                 || not model.ForwardPages.IsEmpty then
                  yield
                      materialButton (fun _ -> dispatch (ShellMsg ForwardPage)) Icons.arrowForward
                          (not model.ForwardPages.IsEmpty
                           && model.CurrentPage.AddToHistory)

              yield
                  materialButton (fun _ -> theme.Toggle()) (if theme.IsLight then Icons.wbSunny else Icons.nightsStay)
                      true

              yield
                  menu [ cls "more"
                         isEnabled (not model.CurrentPage.HideHistoryButtons) ] [
                      menuItem [ cls "material"
                                 header Icons.moreVert ] [
                          menuItem [ header "About Nir Tools..."
                                     MenuItem.onClick (fun _ -> dispatch (ShellMsg ShowAboutPage)) ] []
                      ]
                  ] ]

    grid [ cls "pageHeader"
           toColumnDefinitions "*,auto" ] [
        stackPanelCls "pageHeader" [ titleBlock; yield! descriptionBlock ]
        buttonsAndMenu
    ]

let view (model: Model) (dispatch: Msg -> unit): IView =
    dockPanel [] [
        yield pageHeader model dispatch

        yield
            match model.CurrentPage.PageModel with
            | Start m -> Start.view m (StartMsg >> dispatch)
            | About m -> About.view m (AboutMsg >> dispatch)
            | ApiKey m -> ApiKey.view m (ApiKeyMsg >> dispatch)
            | ErrorModel (_, m) -> Error.view m (ErrorMsg >> dispatch)
            | Plugin { Plugin = plugin; Model = m } -> plugin.View(m, PluginMsg >> dispatch)
    ]

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

type MainWindow() as this =
    inherit HostWindow()

    // Copied from Avalonia.FuncUI.Elmish
    do
        base.Title <- "Nir"
        base.Width <- 800.0
        base.Height <- 600.0
        base.MinWidth <- 800.0
        base.MinHeight <- 600.0
        base.Padding <- Thickness(10.0, 5.0, 10.0, 10.0)

        let ini =
            parseIniFile (getProgramPath () +/ "Nir.ini")

        let theme = theme ini

        let themeState = ThemeSwitcher(this.Styles, theme)

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
        |> Program.withSubscription (fun _ -> themeState.OnChanged(ThemeChanged >> ShellMsg))
#if DEBUG
        |> Program.withTrace (fun msg _ -> printfn "New message: %A" msg)
#endif
        |> Program.runWith (this, this.VisualRoot, ini)
