module Nir.Pages.ModChecker

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

type Msg =
    | FetchGames
    | GotGames of ApiResult<Game []>
    | GameChanged of int
    | OpenFileDialog
    | ChangeFile of string []
    | SelectionChanged of string []
    | CheckFile of string
    | MD5Progress of int64 * int64
    | MD5Complete of Result<string, string>
    | SearchResult of ApiResult<Md5Search []>

module Sub =
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

type ArchiveState =
    | None
    | Hashing
    | Checking
    | Found of Md5Search []
    | NotFound of ApiError

type Model =
    { Window: Window
      Nexus: Nexus
      Games: Game []
      SelectedGames: Game list
      Archive: string
      Hash: string
      State: ArchiveState
      ProgressCurrent: int64
      ProgressMax: int64 }

let init window nexus =
    { Window = window
      Nexus = nexus
      Games = [||]
      SelectedGames = []
      Archive = ""
      Hash = ""
      State = None
      ProgressCurrent = 0L
      ProgressMax = 0L }, Cmd.ofMsg FetchGames

// Update

let update (msg: Msg) (model: Model): Model * Cmd<_> =
    let checkHash model =
        { model with State = Checking },
        Cmd.OfAsync.perform md5Search (model.Nexus, model.SelectedGames.Head.DomainName, model.Hash) SearchResult

    let maybeCheckFile games model =
        let gameChanged = games <> model.SelectedGames

        let newModel =
            if gameChanged then { model with SelectedGames = games } else model

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
                  Games = x.Result |> Array.sortByDescending (fun g -> g.Downloads) }, Cmd.none
        | Error _ -> model, Cmd.none
    | GameChanged n -> maybeCheckFile [ model.Games.[n] ] model
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
            maybeCheckFile model.SelectedGames
                { model with
                      Archive = fileNames.[0]
                      State = None }
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
    | SearchResult r ->
        match r with
        | Ok s ->
            { model with
                  Nexus = s.Nexus
                  State = Found s.Result }, Cmd.none
        | Error e -> { model with State = NotFound e }, Cmd.none

// View

let titleAndSub title subtitle: seq<IView> =
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

let inline processingFile model = model.State = Hashing || model.State = Checking

let modSelector model dispatch =
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

let view (model: Model) (dispatch: Msg -> unit): IView =
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
