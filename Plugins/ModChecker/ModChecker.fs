module Nir.Plugins.ModChecker

open System.IO
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
open Nir.UI

open ModChecker

type private Msg =
    | FetchGames
    | GotGames of ApiResult<Game []>
    | GameChanged of int
    | OpenFileDialog
    | FilesSelected of string []
    | FileTextBoxChanged of string
    | ModInfoMsg of int * ModInfo.Msg

// Model

type private Model =
    { Window: Window
      Nexus: Nexus
      Games: Game []
      GamesByName: Map<string, Game>
      SelectedGames: Game list
      ModInfo: ModInfo.Model list
      ThrottleUpdates: Plugin.ThrottleUpdates }

let private init window nexus throttleUpdates =
    { Window = window
      Nexus = nexus
      ThrottleUpdates = throttleUpdates
      Games = [||]
      GamesByName = Map.empty
      SelectedGames = []
      ModInfo = [] }, Cmd.ofMsg FetchGames

// Update

let inline private processingFile model = Seq.exists ModInfo.processingFile model.ModInfo

let private update (msg: Msg) (model: Model): Model * Cmd<_> =
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
            // If SSE or FoNV are selected, also search the older games whose mods may be used.
            let sle = "Skyrim"
            let sse = "Skyrim Special Edition"
            let fonv = "Fallout New Vegas"
            let fo3 = "Fallout 3"

            let findGames = List.map (fun name -> model.GamesByName.[name])

            let gs =
                let game = model.Games.[n]
                if game.Name = sse then findGames [ sse; sle ]
                elif game.Name = fonv then findGames [ fonv; fo3 ]
                else [ game ]

            { model with SelectedGames = gs }, Cmd.none
        else
            model, Cmd.none
    | OpenFileDialog -> model, Cmd.OfAsync.perform promptModArchive model.Window FilesSelected
    | FilesSelected fileNames ->
        let noFileSelected = (fileNames.Length = 1 && fileNames.[0] = "")

        let sameFiles() =
            let current =
                model.ModInfo
                |> List.map (fun mi -> mi.Archive)
                |> List.toArray
                |> Array.sort

            (fileNames |> Array.sort) = current

        if processingFile model || noFileSelected || sameFiles() then
            model, Cmd.none
        else
            let models, cmds =
                Array.toList fileNames
                |> List.mapi
                    (fun index file ->
                        ModInfo.init model.Nexus model.SelectedGames model.ThrottleUpdates index file)
                |> List.unzip

            let indexCmd n cmd = cmd |> Cmd.map (fun c -> ModInfoMsg(n, c))

            { model with ModInfo = models }, Cmd.batch <| List.mapi indexCmd cmds
    | FileTextBoxChanged fileName ->
        model,
        (if File.Exists(fileName) then Cmd.ofMsg (FilesSelected [| fileName |]) else Cmd.none)
    | ModInfoMsg(id, miMsg) ->
        let miModel, miCmd = ModInfo.update miMsg (List.skip id model.ModInfo).Head

        { model with
              ModInfo =
                  List.mapi (fun i m ->
                      if i = id then miModel else m) model.ModInfo }, Cmd.map ModInfoMsg miCmd


// View

let inline private isGameSelected model = not model.SelectedGames.IsEmpty

let private gameSelector model dispatch =
    if isGameSelected model then
        ComboBox.create
            [ ListBox.margin (0.0, 0.0, 8.0, 0.0)
              ListBox.width 250.0
              ComboBox.height 26.0
              ListBox.maxHeight 2160.0
              ComboBox.virtualizationMode ItemVirtualizationMode.Simple
              ListBox.dataItems model.Games
              ListBox.itemTemplate
                  (DataTemplateView<Game>.create(fun data -> TextBlock.create [ TextBlock.text data.Name ]))
              ComboBox.selectedItem model.SelectedGames.Head
              ListBox.onSelectedIndexChanged (GameChanged >> dispatch)
              ListBox.isEnabled (not <| processingFile model) ] :> IView
    else
        ListBox.create
            [ ListBox.margin (0.0, 0.0, 8.0, 0.0)
              ListBox.width 250.0
              ListBox.maxHeight 2160.0
              ListBox.virtualizationMode ItemVirtualizationMode.Simple
              ListBox.dataItems model.Games
              ListBox.itemTemplate
                  (DataTemplateView<Game>.create(fun data -> TextBlock.create [ TextBlock.text data.Name ]))
              ListBox.onSelectedIndexChanged (GameChanged >> dispatch)
              ListBox.isEnabled (not <| processingFile model) ] :> IView

let private modSelector model dispatch =
    let notProcessingFile = not <| processingFile model
    let baseName path = FileInfo(path).Name
    Grid.create
        [ Grid.column 1
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
                            |> FilesSelected
                            |> dispatch)
                    TextBox.textWrapping TextWrapping.Wrap
                    TextBox.watermark "Mod archive to verify"
                    TextBox.verticalAlignment VerticalAlignment.Center
                    TextBox.acceptsReturn false
                    TextBox.acceptsTab false
                    TextBox.isEnabled notProcessingFile
                    TextBox.text
                        (match model.ModInfo with
                         | [] -> ""
                         | [ mi ] -> baseName mi.Archive
                         | mi :: _multiple -> baseName mi.Archive |> sprintf "%s, ...")
                    TextBox.onTextChanged (FileTextBoxChanged >> dispatch) ]
                Button.create
                    [ Grid.column 1
                      Button.margin (8.0, 0.0, 0.0, 0.0)
                      Button.isDefault true
                      Button.classes [ "default" ]
                      Button.isEnabled notProcessingFile
                      Button.onClick (fun _ -> dispatch OpenFileDialog)
                      Button.content "Browse..." ] ] ]

let private view (model: Model) (dispatch: Dispatch<Msg>): IView =
    let rowDefs =
        if model.Games.Length = 0 || isGameSelected model then
            RowDefinitions("auto, *")
        else
            let defs = RowDefinitions()
            let def = RowDefinition()
            def.MaxHeight <- 2160.0
            defs.Add(def)
            defs

    let (contents: IView list) =
        [ yield! ModInfo.titleAndSub "Nexus Mod Checker"
                     (if processingFile model then "Processing files. Please wait..."
                      elif model.Games.Length = 0 then "Fetching games from Nexus..."
                      elif isGameSelected model then "Drop a mod archive below to verify its contents"
                      else "Select your game below")
          Grid.create
              [ Grid.rowDefinitions rowDefs
                Grid.columnDefinitions (if isGameSelected model then "auto, *" else "*")
                Grid.margin (0.0, 16.0)
                Grid.children
                    (if model.Games.Length = 0 then
                        [ ProgressBar.create
                            [ ProgressBar.dock Dock.Top
                              ProgressBar.isIndeterminate true ] ]
                     elif not <| isGameSelected model then
                         [ gameSelector model dispatch ]
                     else
                         [ gameSelector model dispatch
                           modSelector model dispatch
                           if not <| model.ModInfo.IsEmpty then
                               StackPanel.create
                                   [ Grid.columnSpan 2
                                     Grid.row 1
                                     StackPanel.spacing 8.0
                                     StackPanel.margin (0.0, 8.0)
                                     StackPanel.children
                                         [ yield! List.mapi (fun id mi ->
                                                      ModInfo.view mi (fun msg -> ModInfoMsg(id, msg) |> dispatch))
                                                      model.ModInfo ] ] ]) ] ]

    DockPanel.create
        [ DockPanel.margin 10.0
          DockPanel.children contents ] :> IView

type ModChecker() =
    interface IPlugin with
        member __.Name = "Nexus Mod Checker"

        member __.Description = "Verifies mod archive integrity with Nexus "

        member __.Init(window, nexus, throttleUpdates) = Plugin.mapInit init (window, nexus, throttleUpdates)

        member __.Update(msg, model) = Plugin.mapUpdate update (msg, model)
        member __.View(model, dispatch) = Plugin.mapView view (model, dispatch)
