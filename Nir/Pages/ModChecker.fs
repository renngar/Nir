module Nir.Pages.ModChecker

open FSharp.Data

open Elmish
open Avalonia.Controls
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
    | SelectionChanged of seq<string>
    | CheckFile of string
    | MD5Progress of int64 * int64
    | MD5Complete of string
    | SearchResult of ApiResult<Md5Search []>

module Sub =
    let md5Search file onProgress onComplete dispatch =
        async
            {
            async { Nir.Utility.Md5sum.md5sum file (onProgress >> dispatch) |> (onComplete >> dispatch) } |> Async.Start }
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
      State: ArchiveState
      ProgressCurrent: int64
      ProgressMax: int64 }

let init window nexus =
    { Window = window
      Nexus = nexus
      Games = [||]
      SelectedGames = []
      Archive = ""
      State = None
      ProgressCurrent = 0L
      ProgressMax = 0L }, Cmd.ofMsg FetchGames

// Update

let update (msg: Msg) (model: Model): Model * Cmd<_> =
    let maybeCheckFile model =
        model,
        Cmd.batch
            [ if model.SelectedGames.IsEmpty || model.Archive.Length = 0
              then yield Cmd.none
              else yield Cmd.ofMsg (CheckFile model.Archive) ]
    match msg with
    | FetchGames -> model, Cmd.OfAsync.perform games (model.Nexus, false) GotGames
    | GotGames games ->
        match games with
        | Ok x ->
            let gs = x.Result |> Array.sortByDescending (fun g -> g.Downloads)
            { model with
                  Nexus = x.Nexus
                  Games = gs }, Cmd.none
        // (if model.SelectedGames.IsEmpty then Cmd.ofMsg (GameSelected 0) else Cmd.none)
        | Error _ -> model, Cmd.none
    | GameChanged n -> maybeCheckFile { model with SelectedGames = [ model.Games.[n] ] }
    | OpenFileDialog -> model, Cmd.OfAsync.perform promptModArchive model.Window CheckFile
    | SelectionChanged fileNames ->
        maybeCheckFile
            { model with
                  Archive = Seq.head fileNames
                  State = None }
    | CheckFile file ->
        { model with
              Archive = file
              State = Hashing }, Cmd.ofSub (Sub.md5Search file MD5Progress MD5Complete)
    | MD5Progress(current, max) ->
        { model with
              ProgressCurrent = current
              ProgressMax = max }, Cmd.none
    | MD5Complete hash ->
        { model with State = Checking },
        Cmd.OfAsync.perform md5Search (model.Nexus, model.SelectedGames.Head.DomainName, hash) SearchResult
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
                    TextBlock.text title ]
        yield TextBlock.create
                  [ TextBlock.classes [ "h2" ]
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text subtitle ]
    }

let view (model: Model) (dispatch: Msg -> unit) =
    let isGameSelected = not model.SelectedGames.IsEmpty

    let (contents: IView list) =
        let processingFile = model.State = Hashing || model.State = Checking
        [ yield! titleAndSub "Nexus Mod Checker"
                     (if model.State = Hashing then
                         "Generating file hash. Please wait..."
                      elif model.State = Checking then
                         "Checking with Nexus..."
                      elif model.Games.Length = 0 then
                          "Fetching games from Nexus..."
                      elif isGameSelected then
                          "Drop a mod archive below to verify its contents"
                      else
                          "Select your game below")
          if model.Games.Length = 0 then
              yield ProgressBar.create
                        [ ProgressBar.isIndeterminate true
                          ProgressBar.margin (0.0, 16.0) ]
          else
              yield Grid.create
                        [ Grid.columnDefinitions (if isGameSelected then "auto, *, auto" else "*")
                          Grid.margin (0.0, 16.0)
                          Grid.children
                              [ yield ComboBox.create
                                          [ yield! [ Grid.column 0
                                                     ComboBox.margin (0.0, 0.0, 8.0, 0.0)
                                                     ComboBox.virtualizationMode ItemVirtualizationMode.Simple
                                                     ComboBox.dataItems model.Games
                                                     ComboBox.itemTemplate
                                                         (DataTemplateView<Game>
                                                             .create
                                                                 (fun data ->
                                                                     TextBlock.create [ TextBlock.text data.Name ]))
                                                     ComboBox.onSelectedIndexChanged (GameChanged >> dispatch)
                                                     ComboBox.isEnabled (not processingFile) ]
                                            if model.SelectedGames.IsEmpty then yield ComboBox.height 30.0 ]
                                if isGameSelected then
                                    yield TextBox.create
                                              [ Grid.column 1
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
                                                        |> SelectionChanged
                                                        |> dispatch)
                                                TextBox.textWrapping TextWrapping.Wrap
                                                TextBox.watermark "Mod archive to verify"
                                                // TextBox.height 30.0
                                                TextBox.verticalAlignment VerticalAlignment.Center
                                                TextBox.acceptsReturn false
                                                TextBox.acceptsTab false
                                                TextBox.isEnabled (not processingFile)
                                                // This is tacky, but Ctrl-Insert does not work with Avalonia
                                                TextBox.tip (ToolTip.create [ ToolTip.content [ "Ctrl-V to paste" ] ])
                                                TextBox.text model.Archive
                                                TextBox.onTextChanged
                                                    ((fun s -> seq { s })
                                                     >> SelectionChanged
                                                     >> dispatch) ]
                                    yield Button.create
                                              [ Grid.column 2
                                                Button.margin (8.0, 0.0, 0.0, 0.0)
                                                Button.isDefault true
                                                Button.classes [ "default" ]
                                                Button.isEnabled (not processingFile)
                                                Button.onClick (fun _ -> dispatch OpenFileDialog)
                                                Button.content "Browse..." ] ] ]

          match model.State with
          | None -> ()
          | Hashing ->
              yield ProgressBar.create
                        [ ProgressBar.maximum (double model.ProgressMax)
                          ProgressBar.value (double model.ProgressCurrent) ]
          | Checking -> yield ProgressBar.create [ ProgressBar.isIndeterminate true ]
          | Found rs ->
              let r = rs.[0]
              yield! titleAndSub r.Mod.Name r.Mod.Summary
              yield TextBlock.create
                        [ TextBlock.fontWeight FontWeight.Bold
                          TextBlock.text r.FileDetails.Name ]
          | NotFound { StatusCode = code; Message = msg } ->
              yield TextBlock.create
                        [ TextBlock.classes [ "error" ]
                          TextBlock.text
                              (match code with
                               | HttpStatusCodes.NotFound -> "Unrecognized or Corrupted Archive"
                               | _ -> msg) ] ]

    DockPanel.create
        [ DockPanel.children
            [ StackPanel.create
                [ StackPanel.margin 10.0
                  StackPanel.spacing 4.0
                  StackPanel.children contents ] ] ]
