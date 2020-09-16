module Nir.Plugins.ModChecker

open Avalonia.Controls
open FSharp.Data

open Elmish
open Avalonia.FuncUI.Components
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input // DragDrop
open Avalonia.Layout
open Avalonia.Media

open Nir.Dialogs
open Nir.DSL // FuncUI DragDrop support
open Nir.NexusApi
open Nir.UI

type private Msg =
    | FetchGames
    | GotGames of ApiResult<Game []>
    | GameChanged of int
    | OpenFileDialog
    | ChangeFile of string []
    | SelectionChanged of string []
    | CheckFile of string
    | MD5Progress of int64 * int64
    | MD5Complete of Result<string, string>
    | SearchResult of string list * ApiResult<Md5Search []>

module private Sub =
    let md5Search file onProgress onComplete dispatch =
        async {
            async {
                try
                    Nir.Utility.Md5sum.md5sum file (onProgress >> dispatch) |> Ok
                with e -> e.Message |> Error
                |> (onComplete >> dispatch)
            }
            |> Async.Start
        }
        |> Async.RunSynchronously

// Model

type private ArchiveState =
    | None
    | Hashing
    | Checking
    | Found of Md5Search []
    | NotFound of ApiError

type private Model =
    { Window: Window
      Nexus: Nexus
      Games: Game []
      GamesByName: Map<string, Game>
      SelectedGames: Game list
      Archive: string
      Hash: string
      State: ArchiveState
      ProgressCurrent: int64
      ProgressMax: int64 }

let private init window nexus =
    { Window = window
      Nexus = nexus
      Games = [||]
      GamesByName = Map.empty
      SelectedGames = []
      Archive = ""
      Hash = ""
      State = None
      ProgressCurrent = 0L
      ProgressMax = 0L }, Cmd.ofMsg FetchGames

// Update

let private searchInDomains model gameDomains notFoundModel =
    let search model gameDomain remainingDomains =
        { model with State = Checking },
        Cmd.OfAsync.perform md5Search (model.Nexus, gameDomain, model.Hash)
            (fun result -> SearchResult(remainingDomains, result))

    match Seq.toList gameDomains with
    | [ d ] -> search model d []
    | d :: rest -> search model d rest
    | [] -> notFoundModel, Cmd.none

let private update (msg: Msg) (model: Model): Model * Cmd<_> =
    let checkHash model =
        let domains = Seq.map (fun (g: Game) -> g.DomainName) model.SelectedGames
        searchInDomains model domains model

    let maybeCheckFile model games =
        // If SSE or FoNV are selected, also search the older games whose mods may be used.
        let sle = "Skyrim"
        let sse = "Skyrim Special Edition"
        let fonv = "Fallout New Vegas"
        let fo3 = "Fallout 3"

        let gs =
            match games with
            | [ (game: Game) ] ->
                let findGames = List.map (fun name -> model.GamesByName.[name])
                if game.Name = sse then findGames [ sse; sle ]
                elif game.Name = fonv then findGames [ fonv; fo3 ]
                else games
            | _ -> games

        let gameChanged = gs <> model.SelectedGames

        let newModel =
            if gameChanged then { model with SelectedGames = gs } else model

        if model.Archive.Length = 0 then newModel, Cmd.none
        elif not gameChanged || model.Hash.Length = 0 then newModel, Cmd.ofMsg (CheckFile model.Archive)
        else checkHash newModel

    match msg with
    | FetchGames -> model, Cmd.OfAsync.perform games (model.Nexus, false) GotGames
    | GotGames games ->
        match games with
        | Ok x ->
            { model with
                  Nexus = x.Nexus
                  Games = x.Result |> Array.sortByDescending (fun g -> g.Downloads)
                  GamesByName =
                      x.Result
                      |> Seq.map (fun g -> g.Name, g)
                      |> Map.ofSeq }, Cmd.none
        | Error _ -> model, Cmd.none
    | GameChanged n ->
        if n >= 0 then
            let game = model.Games.[n]
            if game.Name = "Skyrim Special Edition" then
                List.append [ game ]
                    [ for g in model.Games do
                        if g.Name = "Skyrim" then yield g ]
            else
                [ game ]
            |> maybeCheckFile model
        else
            model, Cmd.none
    | OpenFileDialog -> model, Cmd.OfAsync.perform promptModArchive model.Window ChangeFile
    | ChangeFile fileNames ->
        // This will trigger SelectionChanged when the view updates the TextBlock
        { model with
              Archive = Seq.head fileNames
              State = None }, Cmd.none
    | SelectionChanged fileNames ->
        if model.State = Hashing then
            model, Cmd.none
        else
            maybeCheckFile
                { model with
                      Archive = fileNames.[0]
                      State = None } model.SelectedGames
    | CheckFile file ->
        { model with
              Archive = file
              State = Hashing }, Cmd.ofSub (Sub.md5Search file MD5Progress MD5Complete)
    | MD5Progress(current, max) ->
        { model with
              ProgressCurrent = current
              ProgressMax = max }, Cmd.none
    | MD5Complete r ->
        match r with
        | Ok hash -> { model with Hash = hash } |> checkHash
        | Error e ->
            { model with
                  State =
                      NotFound
                          { StatusCode = -1
                            Message = e } }, Cmd.none
    | SearchResult(gameDomains, r) ->
        match r with
        | Ok s ->
            { model with
                  Nexus = s.Nexus
                  State = Found s.Result }, Cmd.none
        | Error e -> searchInDomains model gameDomains { model with State = NotFound e }

// View

let private titleAndSub title subtitle: seq<IView> =
    seq {
        yield TextBlock.create
                  [ TextBlock.classes [ "h1" ]
                    TextBlock.dock Dock.Top
                    TextBlock.text title ]

        yield TextBlock.create
                  [ TextBlock.classes [ "h2" ]
                    TextBlock.dock Dock.Top
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text subtitle ]
    }

let inline private processingFile model = model.State = Hashing || model.State = Checking

let private modSelector model dispatch =
    let notProcessingFile = not <| processingFile model
    Grid.create
        [ Grid.dock Dock.Top
          Grid.columnDefinitions "*, auto"
          Grid.rowDefinitions "auto, *"
          Grid.children
              [ TextBox.create
                  [ Grid.column 0
                    DragDrop.allowDrop true
                    DragDrop.onDragOver (fun e ->
                        e.DragEffects <-
                            if e.Data.Contains(DataFormats.FileNames) then
                                e.DragEffects &&& DragDropEffects.Copy
                            else
                                DragDropEffects.None)
                    DragDrop.onDrop (fun e ->
                        if e.Data.Contains(DataFormats.FileNames) then
                            e.Data.GetFileNames()
                            |> Seq.toArray
                            |> ChangeFile
                            |> dispatch)
                    TextBox.textWrapping TextWrapping.Wrap
                    TextBox.watermark "Mod archive to verify"
                    // TextBox.height 30.0
                    TextBox.verticalAlignment VerticalAlignment.Center
                    TextBox.acceptsReturn false
                    TextBox.acceptsTab false
                    TextBox.isEnabled notProcessingFile
                    // This is tacky, but Ctrl-Insert does not work with Avalonia
                    TextBox.tip (ToolTip.create [ ToolTip.content [ "Ctrl-V to paste" ] ])
                    TextBox.text model.Archive
                    TextBox.onTextChanged (fun s ->
                        [| s |]
                        |> SelectionChanged
                        |> dispatch) ]
                Button.create
                    [ Grid.column 1
                      Button.margin (8.0, 0.0, 0.0, 0.0)
                      Button.isDefault true
                      Button.classes [ "default" ]
                      Button.isEnabled notProcessingFile
                      Button.onClick (fun _ -> dispatch OpenFileDialog)
                      Button.content "Browse..." ] ] ]

let private view (model: Model) (dispatch: Dispatch<Msg>): IView =
    let isGameSelected = not model.SelectedGames.IsEmpty

    let (contents: IView list) =
        [ yield! titleAndSub "Nexus Mod Checker"
                     (if model.State = Hashing then "Generating file hash. Please wait..."
                      elif model.State = Checking then "Checking with Nexus..."
                      elif model.Games.Length = 0 then "Fetching games from Nexus..."
                      elif isGameSelected then "Drop a mod archive below to verify its contents"
                      else "Select your game below")
          if model.Games.Length = 0 then
              yield Grid.create
                        [ Grid.rowDefinitions "auto, *"
                          Grid.children
                              [ yield ProgressBar.create
                                          [ ProgressBar.dock Dock.Top
                                            ProgressBar.isIndeterminate true
                                            ProgressBar.margin (0.0, 16.0) ] ] ]
          else
              let rowDefs = RowDefinitions()
              let def = RowDefinition()
              def.MaxHeight <- 2160.0
              rowDefs.Add(def)

              yield Grid.create
                        [ Grid.columnDefinitions (if isGameSelected then "auto, *" else "*")
                          Grid.rowDefinitions rowDefs
                          Grid.margin (0.0, 16.0)
                          Grid.children
                              [ yield ListBox.create
                                          [ ListBox.margin (0.0, 0.0, 8.0, 0.0)
                                            ListBox.width 250.0
                                            ListBox.maxHeight 2160.0
                                            ListBox.virtualizationMode ItemVirtualizationMode.Simple
                                            ListBox.dataItems model.Games
                                            ListBox.itemTemplate
                                                (DataTemplateView<Game>
                                                    .create(fun data -> TextBlock.create [ TextBlock.text data.Name ]))
                                            ListBox.onSelectedIndexChanged (GameChanged >> dispatch)
                                            ListBox.isEnabled (not <| processingFile model) ]
                                if isGameSelected then
                                    yield StackPanel.create
                                              [ Grid.column 1
                                                StackPanel.spacing 8.0
                                                StackPanel.children
                                                    [ yield modSelector model dispatch
                                                      match model.State with
                                                      | None -> ()
                                                      | Hashing ->
                                                          yield ProgressBar.create
                                                                    [ ProgressBar.dock Dock.Top
                                                                      ProgressBar.maximum (double model.ProgressMax)
                                                                      ProgressBar.value (double model.ProgressCurrent) ]
                                                      | Checking ->
                                                          yield ProgressBar.create
                                                                    [ ProgressBar.dock Dock.Top
                                                                      ProgressBar.isIndeterminate true ]
                                                      | Found rs ->
                                                          let r = rs.[0]
                                                          yield! titleAndSub r.Mod.Name r.Mod.Summary

                                                          yield TextBlock.create
                                                                    [ TextBlock.dock Dock.Top
                                                                      TextBlock.fontWeight FontWeight.Bold
                                                                      TextBlock.text r.FileDetails.Name ]
                                                      | NotFound { StatusCode = code; Message = msg } ->
                                                          yield TextBlock.create
                                                                    [ TextBlock.dock Dock.Top
                                                                      TextBlock.classes [ "error" ]
                                                                      TextBlock.text
                                                                          (match code with
                                                                           | HttpStatusCodes.NotFound ->
                                                                               "Unrecognized or Corrupted Archive"
                                                                           | _ -> msg) ] ] ] ] ] ]

    DockPanel.create
        [ DockPanel.margin 10.0
          DockPanel.children contents ] :> IView

type ModChecker() =
    interface IPlugin with
        member __.Name = "Nexus Mod Checker"
        member __.Description = "Verifies mod archive integrity with Nexus "
        member __.Init(window, nexus) = Plugin.mapInit init (window, nexus)
        member __.Update(msg, model) = Plugin.mapUpdate update (msg, model)
        member __.View(model, dispatch) = Plugin.mapView view (model, dispatch)
