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

type private ModInfo = Map<int, Model>

type private Model =
    { Window: Window
      Nexus: Nexus
      Properties: Properties
      Games: Game []
      GamesByName: Map<string, Game>
      SelectedGames: Game list
      ModInfo: ModInfo
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
    let runParallel (computations: seq<Async<unit>>): unit =
        // This process may not be processor bound, but this is a good place to start for max parallelism.
        (computations, Environment.ProcessorCount)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

    let checkFiles fileNames nexus selectedGames throttleUpdates dispatch =
        async {
            fileNames
            |> Seq.mapi (fun index file ->
                Sub.processFile nexus selectedGames throttleUpdates index file (ModInfoMsg >> dispatch))
            |> runParallel
        }
        |> Async.Start

    let recheckFiles (modInfos: ModInfo) nexus selectedGames dispatch =
        async {
            modInfos
            |> Seq.map (fun mi -> Sub.reprocessFile nexus selectedGames mi.Value (ModInfoMsg >> dispatch))
            |> runParallel
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
    let noOp = Plugin.NoOp

    let noFileSelected (fileNames: string []) =
        (fileNames.Length = 1 && fileNames.[0] = "")

    let getFileNames () =
        model.ModInfo
        |> Map.toSeq
        |> Seq.map (fun (_, mi) -> mi.Archive)
        |> Seq.toArray

    match msg with
    | FetchGames -> model, Cmd.OfAsync.perform model.Nexus.Games false GotGames, noOp
    | GotGames games ->
        match games with
        | Ok gs ->
            { model with
                  Games = Array.sortByDescending (fun g -> g.Downloads) gs
                  GamesByName = gs |> Seq.map (fun g -> g.Name, g) |> Map.ofSeq },
            Cmd.none,
            noOp
        | Error _ -> model, Cmd.none, noOp
    | GameChanged n ->
        if n >= 0 then
            // If SSE or FoNV are selected, also search the older games whose mods may be used.
            let sle = "Skyrim"
            let sse = "Skyrim Special Edition"
            let fonv = "Fallout New Vegas"
            let fo3 = "Fallout 3"

            let findGames =
                List.map (fun name -> model.GamesByName.[name])

            let newModel =
                { model with
                      SelectedGames =
                          let game = model.Games.[n]

                          if game.Name = sse then findGames [ sse; sle ]
                          elif game.Name = fonv then findGames [ fonv; fo3 ]
                          else [ game ] }

            let cmd =
                let fileNames = getFileNames ()

                if processingFile newModel
                   || noFileSelected fileNames then
                    Cmd.none
                else
                    Cmd.ofSub (Sub.recheckFiles newModel.ModInfo newModel.Nexus newModel.SelectedGames)

            newModel, cmd, noOp
        else
            model, Cmd.none, noOp
    | OpenFileDialog ->
        let dir = getModDir model
        model, Cmd.OfAsync.perform promptModArchives (model.Window, dir) FilesSelected, noOp
    | FilesSelected fileNames ->
        let sameFiles () =
            let current = getFileNames () |> Array.sort

            (fileNames |> Array.sort) = current

        if processingFile model
           || noFileSelected fileNames
           || sameFiles () then
            model, Cmd.none, noOp
        else
            let directory = Path.GetDirectoryName fileNames.[0]

            let cmd =
                Cmd.ofSub (Sub.checkFiles fileNames model.Nexus model.SelectedGames model.ThrottleUpdates)

            if getModDir model = directory then
                { model with ModInfo = Map.empty }, cmd, noOp
            else
                let newModel =
                    { model with
                          ModInfo = Map.empty
                          Properties = setProperty model.Properties modDirProperty directory }

                newModel, cmd, Plugin.SaveProperties newModel.Properties
    | FileTextBoxChanged fileName ->
        model, (if File.Exists(fileName) then Cmd.ofMsg (FilesSelected [| fileName |]) else Cmd.none), noOp
    | ModInfoMsg (id, miMsg) ->
        match miMsg with
        | ModInfoUpdate miModel ->
            { model with
                  ModInfo = model.ModInfo.Add(id, miModel) },
            Cmd.none,
            noOp


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

let private modPanel games (searchResults: seq<string * Md5Search []>) =
    stackPanelCls
        "mod"
        [ let _, r = Seq.head searchResults
          let m = r.[0].Mod
          let rowDefs = RowDefinitions()
          rowDefs.AddRange(Seq.init (Seq.length searchResults) (fun _ -> RowDefinition(GridLength.Auto)))

          yield!
              modHeader
              <|| if m.Available then
                      m.Name, m.Summary
                  else
                      let game =
                          Array.find (fun (g: Game) -> g.Id = m.GameId) games

                      sprintf "%s Mod %d Unavailable" game.Name m.ModId, sprintf "It is %s" (statusText r.[0])

          yield
              grid
                  [ cls "modDetails"
                    toColumnDefinitions "*,*,*"
                    rowDefinitions rowDefs ]
                  (Seq.sortBy fst searchResults
                   |> Seq.mapi (fun i (archive, results) ->
                       [ let baseName = Path.baseName archive

                         yield
                             textBlock
                                 [ row i
                                   cls "modDetails"
                                   md5Result results.[0] |> toTip ]
                                 baseName

                         yield
                             textBlock [ row i
                                         column 1
                                         TextBlock.textAlignment TextAlignment.Right ]
                             <| String.concat "\n"
                                    (Seq.mapi (fun j _ -> if j = 0 then " matches " else " and ") results)

                         yield
                             textBlock [ row i; column 2 ]
                             <| String.concat "\n"
                                    (Seq.map (fun (r: Md5Search) ->
                                        (sprintf
                                            "%d %s -- %s FILES: %s %s %s"
                                             r.Mod.ModId
                                             r.Mod.Name
                                             (match r.FileDetails.CategoryName with
                                              | Some s -> s
                                              | None -> "MAIN???")
                                             r.FileDetails.Name
                                             (r.FileDetails.UploadedTime.ToLocalTime().ToString())
                                             r.FileDetails.Version)) results) ])
                   |> List.concat) ]

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
                    |> Seq.map (fun m ->
                        match m.State with
                        | Found s -> m.Archive, s
                        | _ -> failwith "should not happen")
                    |> Seq.groupBy (fun (_, xs) ->
                        // We stop querying different games once a match is found, so all matches should be for the
                        // same game.
                        let gameId = xs.[0].Mod.GameId

                        match games.TryGetValue gameId with
                        | true, name -> name
                        | false, _ ->
                            let name =
                                (model.SelectedGames
                                 |> List.find (fun g -> g.Id = gameId))
                                    .Name

                            games <- games.Add(gameId, name)
                            name)
                    |> Seq.sortBy fst
                    |> Seq.map (fun (gameName, results) ->
                        stackPanelCls
                            "game"
                            [ yield textBlockCls "gameName" (sprintf "%s Mods" gameName)
                              yield!
                                  results
                                  |> Seq.groupBy (fun (_, result) ->
                                      // TODO: Deal with files matching multiple mods
                                      let m = result.[0].Mod
                                      m.Available, m.Status, m.GameId, m.ModId, m.Name)
                                  |> Seq.sortBy (fun ((_, _, _, _, name), _) -> name)
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
