module Nir.Plugins.ModChecker

open System
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
open Nir.Utility.INI

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
      Properties: Properties
      Games: Game []
      GamesByName: Map<string, Game>
      SelectedGames: Game list
      ModInfo: Map<int, ModInfo.Model>
      ThrottleUpdates: Plugin.ThrottleUpdates }

let private init window nexus properties throttleUpdates =
    { Window = window
      Nexus = nexus
      Properties = properties
      ThrottleUpdates = throttleUpdates
      Games = [||]
      GamesByName = Map.empty
      SelectedGames = []
      ModInfo = Map.empty },
    Cmd.ofMsg FetchGames

module private Sub =
    let checkFiles fileNames nexus selectedGames throttleUpdates dispatch =
        async {
            fileNames
            |> Seq.mapi (fun index file ->
                Sub.processFile nexus selectedGames throttleUpdates index file (ModInfoMsg >> dispatch))
            // This process may not be processor bound, but this is a good place to start for max parallelism.
            |> fun xs ->
                let cpus = Environment.ProcessorCount
                xs, cpus
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore
        }
        |> Async.Start

// Update

let inline private processingFile model =
    Map.exists (fun _ x -> x.State = Hashing) model.ModInfo

let modDirProperty = "ModDir"

let private getModDir model =
    match tryProperty modDirProperty model.Properties with
    | Some p -> p.Value
    | None -> (Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments))

let private update (msg: Msg) (model: Model): Model * Cmd<_> * Plugin.ExternalMsg =
    let NoOp = Plugin.NoOp

    match msg with
    | FetchGames -> model, Cmd.OfAsync.perform games (model.Nexus, false) GotGames, NoOp
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
            Cmd.none,
            NoOp
        | Error _ -> model, Cmd.none, NoOp
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

            { model with SelectedGames = gs }, Cmd.none, NoOp
        else
            model, Cmd.none, NoOp
    | OpenFileDialog ->
        let dir = getModDir model
        model, Cmd.OfAsync.perform promptModArchives (model.Window, dir) FilesSelected, NoOp
    | FilesSelected fileNames ->
        let noFileSelected =
            (fileNames.Length = 1 && fileNames.[0] = "")

        let sameFiles () =
            let current =
                model.ModInfo
                |> Map.toSeq
                |> Seq.map (fun (_, mi) -> mi.Archive)
                |> Seq.toArray
                |> Array.sort

            (fileNames |> Array.sort) = current

        if processingFile model
           || noFileSelected
           || sameFiles () then
            model, Cmd.none, NoOp
        else
            let directory = Path.GetDirectoryName fileNames.[0]

            let cmd =
                Cmd.ofSub (Sub.checkFiles fileNames model.Nexus model.SelectedGames model.ThrottleUpdates)

            if getModDir model = directory then
                { model with ModInfo = Map.empty }, cmd, NoOp
            else
                let newModel =
                    { model with
                          ModInfo = Map.empty
                          Properties = setProperty model.Properties modDirProperty directory }

                newModel, cmd, Plugin.SaveProperties newModel.Properties
    | FileTextBoxChanged fileName ->
        model, (if File.Exists(fileName) then Cmd.ofMsg (FilesSelected [| fileName |]) else Cmd.none), NoOp
    | ModInfoMsg (id, miMsg) ->
        match miMsg with
        | ModInfoUpdate miModel ->
            { model with
                  ModInfo = model.ModInfo.Add(id, miModel) },
            Cmd.none,
            NoOp


// View

let inline private isGameSelected model = not model.SelectedGames.IsEmpty

let private gameSelector model dispatch =
    let gameName (data: Game): IView = textBlock [] data.Name

    if not (isGameSelected model) then
        listBox [ cls "gameSelector"
                  ListBox.virtualizationMode ItemVirtualizationMode.Simple
                  dataItems model.Games
                  itemTemplate gameName
                  onSelectedIndexChanged (GameChanged >> dispatch)
                  isEnabled (not <| processingFile model) ]
    else
        comboBox [ cls "gameSelector"
                   ComboBox.virtualizationMode ItemVirtualizationMode.Simple
                   dataItems model.Games
                   itemTemplate gameName
                   selectedItem model.SelectedGames.Head
                   onSelectedIndexChanged (GameChanged >> dispatch)
                   isEnabled (not <| processingFile model) ]

let private modSelector model dispatch =
    let notProcessingFile = not <| processingFile model

    grid [ cls "modSelector"
           column 1
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
            (let firstArchive () =
                Path.baseName (Seq.head model.ModInfo).Value.Archive

             match model.ModInfo.Count with
             | 0 -> ""
             | 1 -> firstArchive ()
             | _ -> firstArchive () |> sprintf "%s, ...")

        textButton
            [ column 1
              isDefault true
              isEnabled notProcessingFile
              onClick (fun _ -> dispatch OpenFileDialog) ]
            "Browse..."
    ]

let private modHeader name summary =
    [ textBlockCls "h1" name
      textBlockCls "modSummary" summary ]

// The JSON provider thinks the MD5 output is a GUID, remove the dashes
let private md5Result (result: Md5Search) =
    result.FileDetails.Md5.ToString().Replace("-", "")

// Convert things like "under_moderation" to "under moderation"
let private statusText (result: Md5Search) = result.Mod.Status.Replace("_", " ")

let private modPanel games (searchResults: seq<Md5Search>) =
    stackPanelCls
        "mod"
        [ let r = Seq.head searchResults
          let m = r.Mod

          yield!
              modHeader
              <|| if m.Available then
                      m.Name, m.Summary
                  else
                      let game =
                          Array.find (fun (g: Game) -> g.Id = m.GameId) games

                      sprintf "%s Mod %d Unavailable" game.Name m.ModId, sprintf "It is %s" (statusText r)

          yield!
              searchResults
              |> Seq.map (fun result ->
                  textBlock [ cls "modDetails"
                              md5Result result |> toTip ]
                  <| sprintf "%s — %s" result.FileDetails.Name result.FileDetails.FileName) ]

let private modInfo (model: Model) (dispatch: Msg -> unit): IView =
    let mutable games = Map.empty

    stackPanelCls
        "modInfo"
        [ for (group, infos) in groupByState model.ModInfo |> Seq.sortBy fst do
            // Output the group header, if any
            match groupHeader group infos with
            | Some header -> yield header
            | None -> ()

            yield!
                if group = FoundGroup then
                    infos
                    |> Seq.map (fun { State = state } ->
                        match state with
                        | Found s -> s
                        | _ -> failwith "should not happen")
                    // Get the first (and likely only) match where the file was found
                    // TODO Handle the case of multiple matches, such as occurs with a zero-length file
                    |> Seq.map Array.head
                    |> Seq.groupBy (fun x ->
                        let id = x.Mod.GameId

                        match games.TryGetValue id with
                        | true, name -> name
                        | false, _ ->
                            let name =
                                (model.SelectedGames
                                 |> List.find (fun g -> g.Id = id))
                                    .Name

                            games <- games.Add(id, name)
                            name)
                    |> Seq.map (fun (gameName, results) ->
                        stackPanelCls
                            "game"
                            [ yield textBlockCls "gameName" (sprintf "%s Mods" gameName)
                              yield!
                                  results
                                  |> Seq.groupBy (fun result ->
                                      let m = result.Mod
                                      m.Available, m.Status, m.GameId, m.ModId, m.Name)
                                  |> Seq.map (fun (_, searchResults) -> modPanel model.Games searchResults) ])
                else
                    // Output the file info
                    Seq.sortBy orderBy infos
                    |> Seq.map (fun mi -> view mi (fun msg -> ModInfoMsg(mi.Id, msg) |> dispatch)) ]

let private view (model: Model) (dispatch: Dispatch<Msg>): IView =
    dockPanel [ cls "modChecker" ] [
        yield
            pageHeader
                "Nexus Mod Checker"
                (if processingFile model then "Processing files. Please wait..."
                 elif model.Games.Length = 0 then "Fetching games from Nexus..."
                 elif isGameSelected model then "Drop a mod archive below to verify its contents"
                 else "Select your game below")
        if model.Games.Length = 0 then
            yield!
                [ progressBar [ dock Dock.Top
                                isIndeterminate true ]

                  // Let's the progress bar take it's natural height and fills the rest with nothing
                  textBlock [] "" ]
        elif not <| isGameSelected model then
            yield gameSelector model dispatch
        else
            yield
                grid [ dock Dock.Top
                       cls "selectors"
                       toColumnDefinitions "auto,*"
                       toRowDefinitions "auto,*" ] [
                    yield gameSelector model dispatch
                    yield modSelector model dispatch
                ]

            if model.ModInfo.IsEmpty |> not
            then yield scrollViewer [] <| modInfo model dispatch
    ]

type ModChecker() =
    interface IPlugin with
        member __.Name = "Nexus Mod Checker"

        member __.Description =
            "Verifies mod archive integrity with Nexus "

        member __.DarkStyle = "avares://ModChecker/ModChecker.xaml"
        member __.LightStyle = "avares://ModChecker/ModChecker.xaml"

        member __.Init(window, nexus, initialProperties, throttleUpdates) =
            Plugin.mapInit init (window, nexus, initialProperties, throttleUpdates)

        member __.Update(msg, model) = Plugin.mapUpdate update (msg, model)
        member __.View(model, dispatch) = Plugin.mapView view (model, dispatch)
