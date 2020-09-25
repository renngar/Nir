module Nir.Plugins.ModChecker

open System.IO
open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input // DragDrop
open Avalonia.Layout
open Avalonia.Media

open Nir.Dialogs
open Nir.NexusApi
open Nir.UI
open Nir.UI.Controls
open Nir.Utility

open ModChecker
open ModChecker.ModInfo

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
      ModInfo = [] },
    Cmd.ofMsg FetchGames

// Update

let inline private processingFile model = Seq.exists processingFile model.ModInfo

let private update (msg: Msg) (model: Model): Model * Cmd<_> =
    match msg with
    | FetchGames -> model, Cmd.OfAsync.perform games (model.Nexus, false) GotGames
    | GotGames games ->
        match games with
        | Ok x ->
            { model with
                  Nexus = x.Nexus
                  Games =
                      x.Result
                      |> Array.sortByDescending (fun g -> g.Downloads)
                  GamesByName =
                      x.Result
                      |> Seq.map (fun g -> g.Name, g)
                      |> Map.ofSeq },
            Cmd.none
        | Error _ -> model, Cmd.none
    | GameChanged n ->
        if n >= 0 then
            // If SSE or FoNV are selected, also search the older games whose mods may be used.
            let sle = "Skyrim"
            let sse = "Skyrim Special Edition"
            let fonv = "Fallout New Vegas"
            let fo3 = "Fallout 3"

            let findGames =
                List.map (fun name -> model.GamesByName.[name])

            let gs =
                let game = model.Games.[n]

                if game.Name = sse then findGames [ sse; sle ]
                elif game.Name = fonv then findGames [ fonv; fo3 ]
                else [ game ]

            { model with SelectedGames = gs }, Cmd.none
        else
            model, Cmd.none
    | OpenFileDialog -> model, Cmd.OfAsync.perform promptModArchives model.Window FilesSelected
    | FilesSelected fileNames ->
        let noFileSelected =
            (fileNames.Length = 1 && fileNames.[0] = "")

        let sameFiles () =
            let current =
                model.ModInfo
                |> List.map (fun mi -> mi.Archive)
                |> List.toArray
                |> Array.sort

            (fileNames |> Array.sort) = current

        if processingFile model
           || noFileSelected
           || sameFiles () then
            model, Cmd.none
        else
            let models, commands =
                Array.toList fileNames
                |> List.mapi (fun index file ->
                    ModInfo.init model.Nexus model.SelectedGames model.ThrottleUpdates index file)
                |> List.unzip

            let indexCmd n cmd =
                cmd |> Cmd.map (fun c -> ModInfoMsg(n, c))

            { model with ModInfo = models }, Cmd.batch <| List.mapi indexCmd commands
    | FileTextBoxChanged fileName ->
        model, (if File.Exists(fileName) then Cmd.ofMsg (FilesSelected [| fileName |]) else Cmd.none)
    | ModInfoMsg (id, miMsg) ->
        let miModel, miCmd =
            update miMsg (List.skip id model.ModInfo).Head

        { model with
              ModInfo = List.mapi (fun i m -> if i = id then miModel else m) model.ModInfo },
        Cmd.map ModInfoMsg miCmd


// View

let inline private isGameSelected model = not model.SelectedGames.IsEmpty

let private gameSelector model dispatch =
    let gameName (data: Game): IView = textBlock [] data.Name

    if isGameSelected model then
        comboBox [ marginRight 8.0
                   width 250.0
                   height 26.0
                   maxHeight 2160.0
                   ComboBox.virtualizationMode ItemVirtualizationMode.Simple
                   dataItems model.Games
                   itemTemplate gameName
                   selectedItem model.SelectedGames.Head
                   onSelectedIndexChanged (GameChanged >> dispatch)
                   isEnabled (not <| processingFile model) ]
    else
        listBox [ marginRight 8.0
                  width 250.0
                  maxHeight 2160.0
                  ListBox.virtualizationMode ItemVirtualizationMode.Simple
                  dataItems model.Games
                  itemTemplate gameName
                  onSelectedIndexChanged (GameChanged >> dispatch)
                  isEnabled (not <| processingFile model) ]

let private modSelector model dispatch =
    let notProcessingFile = not <| processingFile model

    grid [ column 1
           toColumnDefinitions "*, auto"
           toRowDefinitions "auto, *" ] [
        textBox
            [ column 0
              allowDrop true
              onDragOver (fun e ->
                  e.DragEffects <-
                      if e.Data.Contains(DataFormats.FileNames) then
                          e.DragEffects &&& DragDropEffects.Copy
                      else
                          DragDropEffects.None)
              onDrop (fun e ->
                  if e.Data.Contains(DataFormats.FileNames) then
                      e.Data.GetFileNames()
                      |> Seq.toArray
                      |> FilesSelected
                      |> dispatch)
              textWrapping TextWrapping.Wrap
              TextBox.watermark "Mod archive to verify"
              verticalAlignment VerticalAlignment.Center
              acceptsReturn false
              acceptsTab false
              isEnabled notProcessingFile
              onTextChanged (FileTextBoxChanged >> dispatch) ]
            (match model.ModInfo with
             | [] -> ""
             | [ mi ] -> Path.baseName mi.Archive
             | mi :: _multiple -> Path.baseName mi.Archive |> sprintf "%s, ...")

        textButton
            [ column 1
              marginLeft 8.0
              isDefault true
              classes [ "default" ]
              isEnabled notProcessingFile
              onClick (fun _ -> dispatch OpenFileDialog) ]
            "Browse..."
    ]

let private modInfo modInfos dispatch =
    stackPanel [ spacing 8.0 ] [
        for (group, infos) in groupByState modInfos |> List.sortBy fst do
            // Output the group header, if any
            match groupHeader group infos with
            | Some header -> yield header
            | None -> ()

            if group = FoundGroup then
                let filesByMod =
                    infos
                    |> List.map (fun mi ->
                        match mi.State with
                        | Found x -> x
                        | _ -> failwith "should not happen")
                    // Get the first (and likely only) match where the file was found
                    // TODO Handle the case of multiple matches, such as occurs with a zero-length file
                    |> List.map Array.head
                    |> List.groupBy (fun result -> result.Mod)

                for (_, searchResults) in filesByMod do
                    yield! modHeader searchResults.Head

                    for result in searchResults do
                        yield modDetail result

            // Output the file info
            for mi in List.sortBy orderBy infos do
                match mi.State with
                | Found _ -> () // Handled separately above
                | _ -> yield view mi (fun msg -> ModInfoMsg(mi.Id, msg) |> dispatch)
    ]

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

    dockPanel [ margin 10.0 ]
    <| [ yield!
             titleAndSub
                 "Nexus Mod Checker"
                 (if processingFile model then "Processing files. Please wait..."
                  elif model.Games.Length = 0 then "Fetching games from Nexus..."
                  elif isGameSelected model then "Drop a mod archive below to verify its contents"
                  else "Select your game below")
         yield
             grid
                 [ rowDefinitions rowDefs
                   toColumnDefinitions (if isGameSelected model then "auto, *" else "*")
                   marginTop 16.0 ]
                 (if model.Games.Length = 0 then
                     [ progressBar [ dock Dock.Top
                                     isIndeterminate true ] ]
                  elif not <| isGameSelected model then
                      [ gameSelector model dispatch ]
                  else
                      [ gameSelector model dispatch
                        modSelector model dispatch
                        if model.ModInfo.IsEmpty |> not then
                            scrollViewer [ columnSpan 2
                                           row 1
                                           marginTop 8.0 ]
                            <| modInfo model.ModInfo dispatch ]) ]

type ModChecker() =
    interface IPlugin with
        member __.Name = "Nexus Mod Checker"

        member __.Description =
            "Verifies mod archive integrity with Nexus "

        member __.Init(window, nexus, throttleUpdates) =
            Plugin.mapInit init (window, nexus, throttleUpdates)

        member __.Update(msg, model) = Plugin.mapUpdate update (msg, model)
        member __.View(model, dispatch) = Plugin.mapView view (model, dispatch)
