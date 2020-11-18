module Nir.Plugins.ModChecker

open System
open System.Globalization
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
open Nir.Web

open ModChecker
open ModChecker.ModInfo

let private name = "Nexus Mod Checker"

type private Msg =
    | FetchGames
    | GotGames of ApiResult<Game []>
    | GameChanged of int
    | OpenFileDialog
    | FilesSelected of string []
    | FileTextBoxChanged of string
    | ModInfoMsg of int * ModInfo.Msg
    interface IPluginMsg

// Model
type private ModInfo = Map<int, Model>

module internal Property =
    let modDirectory = "ModDir"
    let games = "Games"
    let gameDelimiters = [ ", "; "," ]

type GamesByName = Map<string, Game>

type private Model =
    { Window: Window
      Nexus: Nexus
      Properties: Properties
      Games: Game []
      GamesByName: GamesByName
      SelectedGames: Game []
      ModInfo: ModInfo
      ThrottleUpdates: Plugin.ThrottleUpdates }

    interface IPluginModel with
        member __.Title = name

        member this.Description =
            if this.ProcessingFile then "Processing files. Please wait..."
            elif this.Games.Length = 0 then "Fetching games from Nexus..."
            elif this.IsGameSelected then "Drop a mod archive below to verify its contents"
            else "Select your game below"

    member this.IsGameSelected
        with internal get () = this.SelectedGames.Length > 0

    member this.ProcessingFile
        with internal get () =
            Map.exists (fun _ x -> x.State = Hashing) this.ModInfo

    member this.ModDirectory
        with internal get () =
            tryPropertyValue Property.modDirectory this.Properties
            |> Option.defaultValue (Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments))

let private findGames (gamesByName: GamesByName) (games: string []): Game [] =
    Array.map (fun name -> gamesByName.[name]) games

let private init window nexus properties throttleUpdates =
    { Window = window
      Nexus = nexus
      Properties = properties
      ThrottleUpdates = throttleUpdates
      Games = [||]
      GamesByName = Map.empty
      SelectedGames = Array.empty
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

    let recheckFiles (modInfos: ModInfo) (nexus: Nexus) (selectedGames: Game []) (dispatch: Msg -> unit): unit =
        async {
            modInfos
            |> Seq.map (fun mi -> Sub.reprocessFile nexus selectedGames mi.Value (ModInfoMsg >> dispatch))
            |> runParallel
        }
        |> Async.Start

// Update

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
            let gamesByName =
                gs |> Seq.map (fun g -> g.Name, g) |> Map.ofSeq

            { model with
                  Games = Array.sortByDescending (fun g -> g.Downloads) gs
                  GamesByName = gamesByName
                  SelectedGames =
                      getPropertyValues Property.games model.Properties Property.gameDelimiters
                      |> findGames gamesByName },
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

            let game = model.Games.[n].Name

            let selectedGames =
                if game = sse then [| sse; sle |]
                elif game = fonv then [| fonv; fo3 |]
                else [| game |]

            let newModel =
                { model with
                      SelectedGames = findGames model.GamesByName selectedGames
                      Properties =
                          setPropertyList model.Properties Property.games Property.gameDelimiters.Head selectedGames }

            let cmd =
                let fileNames = getFileNames ()

                if newModel.ProcessingFile
                   || noFileSelected fileNames then
                    Cmd.none
                else
                    Cmd.ofSub (Sub.recheckFiles newModel.ModInfo newModel.Nexus newModel.SelectedGames)

            newModel, cmd, Plugin.SaveProperties newModel.Properties
        else
            model, Cmd.none, noOp
    | OpenFileDialog ->
        model, Cmd.OfAsync.perform promptModArchives (model.Window, model.ModDirectory) FilesSelected, noOp
    | FilesSelected fileNames ->
        let sameFiles () =
            let current = getFileNames () |> Array.sort

            (fileNames |> Array.sort) = current

        if model.ProcessingFile
           || noFileSelected fileNames
           || sameFiles () then
            model, Cmd.none, noOp
        elif fileNames.Length = 0 then
            model, Cmd.none, Plugin.NoOp
        else
            let directory = Path.GetDirectoryName fileNames.[0]

            let cmd =
                Cmd.ofSub (Sub.checkFiles fileNames model.Nexus model.SelectedGames model.ThrottleUpdates)

            if model.ModDirectory = directory then
                { model with ModInfo = Map.empty }, cmd, noOp
            else
                let newModel =
                    { model with
                          ModInfo = Map.empty
                          Properties = setProperty model.Properties Property.modDirectory directory }

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

let private gameSelector (model: Model) dispatch =
    let gameName (data: Game): IView = textBlock [] data.Name

    if not <| model.IsGameSelected then
        listBox [ cls "gameSelector"
                  ListBox.virtualizationMode ItemVirtualizationMode.Simple
                  dataItems model.Games
                  itemTemplate gameName
                  onSelectedIndexChanged (GameChanged >> dispatch)
                  isEnabled (not <| model.ProcessingFile) ]
    else
        comboBox [ cls "gameSelector"
                   ComboBox.virtualizationMode ItemVirtualizationMode.Simple
                   dataItems model.Games
                   itemTemplate gameName
                   selectedItem model.SelectedGames.[0]
                   onSelectedIndexChanged (GameChanged >> dispatch)
                   isEnabled (not <| model.ProcessingFile) ]

let private modSelector (model: Model) dispatch =
    let notProcessingFile = not <| model.ProcessingFile

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
              TextBox.watermark "Paste or drop mod files here"
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

// The JSON provider thinks the MD5 output is a GUID, remove the dashes
let private md5Result (result: Md5Search) =
    result.FileDetails.Md5.ToString().Replace("-", "")

let private modHeader name = textBlockCls "h1" name

let private fileHeader archive =
    textBlockCls "modDetails" (Path.baseName archive)

let private findGameById games gameId =
    Array.find (fun (g: Game) -> g.Id = gameId) games

let private titleCase (str: string) =
    str.ToLower()
    |> CultureInfo.CurrentCulture.TextInfo.ToTitleCase

let replaceUnderlines (str: string) = str.Replace("_", " ")

/// Strips the markup and leading or trailing whitespace (like newlines) from a string
let private stripMarkup str =
    BBCode.strip str
    |> stripHtml
    |> (fun (str: string) -> str.Trim())

let private fileDetails allGames (``mod``: Mod) archive (results: Md5Search []) =
    let baseName = Path.baseName archive
    let modsGameId = ``mod``.GameId

    stackPanel [] [
        for (gameId, modId, category), rs in Array.groupBy (fun (r: Md5Search) ->
                                                 r.Mod.GameId, r.Mod.ModId, r.FileDetails.CategoryName) results do
            if gameId <> modsGameId || modId <> ``mod``.ModId then
                let resultGame = findGameById allGames gameId
                yield textBlock [] (sprintf "%s Mod %d" resultGame.Name modId)

            let borderedText ``class`` text =
                border
                    [ yield cls ``class``
                      if String.IsNullOrEmpty text then yield Border.height 0.0 ]
                    (textBlockCls ``class`` text)

            match category with
            | Some cat -> yield borderedText "modCategory" (titleCase cat |> sprintf "%s Files")
            | None -> ()

            for r in rs do
                yield
                    expander
                        [ classes [ "adornRight"; "fileExpander" ]
                          isExpanded true
                          Expander.header
                              (grid [ cls "fileExpanderHeader"
                                      toColumnDefinitions "*,auto,auto,auto"
                                      toRowDefinitions "*,*" ] [
                                  let text c r text =
                                      textBlock [ cls "small"; row r; column c ] text

                                  if r.FileDetails.Name <> baseName
                                  then yield textBlock [ rowSpan 2 ] r.FileDetails.Name

                                  yield text 1 0 "MD5 Hash"
                                  yield text 1 1 (md5Result r)
                                  yield text 2 0 "Date Uploaded"
                                  yield text 2 1 (r.FileDetails.UploadedTime.ToLocalTime().ToString("f"))
                                  yield text 3 0 "Version"
                                  yield text 3 1 r.FileDetails.Version
                               ]) ]
                        (borderedText "modDescription" (stripMarkup r.FileDetails.Description))
    ]

// Convert things like "under_moderation" to "under moderation"
let private statusText (result: Md5Search) = result.Mod.Status |> replaceUnderlines

let private modPanel games (searchResults: seq<string * Md5Search []>) =
    stackPanelCls
        "mod"
        [ let _, r = Seq.head searchResults
          let m = r.[0].Mod
          let rowDefs = RowDefinitions()
          let hasSummary = not (String.IsNullOrEmpty m.Summary)
          let summaryOffset = if hasSummary then 1 else 0
          let nRows = Seq.length searchResults + summaryOffset
          rowDefs.AddRange(Seq.init nRows (fun _ -> RowDefinition(GridLength.Auto)))

          expander
              [ cls "adornRight"
                isExpanded true
                Expander.header
                    (modHeader
                     <| if m.Available then
                         m.Name
                        else
                            let game = findGameById games m.GameId

                            sprintf "%s Mod %d Unavailable" game.Name m.ModId) ]

              (grid
                  [ cls "modDetails"
                    toColumnDefinitions "*,*,*"
                    rowDefinitions rowDefs ]
                   (Seq.sortBy fst searchResults
                    |> Seq.mapi (fun i (archive, results) ->
                        [ expander
                            [ cls "adornRight"
                              row i
                              Expander.header (fileHeader archive) ]
                              (fileDetails games m archive results) ])
                    |> List.concat)) ]

let inline private fifth (_, _, _, _, e) = e

let private gamePanel games (gameName, results) =
    stackPanelCls
        "game"
        [ expander
            [ classes [ "game"; "large" ]
              isExpanded true
              Expander.header (textBlockCls "gameName" (sprintf "%s Mods" gameName)) ]
              (stackPanel [] [
                  yield!
                      results
                      |> Seq.groupBy (fun (_, result: Md5Search []) ->
                          // TODO: Deal with files matching multiple mods
                          let m = result.[0].Mod
                          m.Available, m.Status, m.GameId, m.ModId, m.Name)
                      |> Seq.sortBy (fst >> fifth) // Sort by name
                      |> Seq.map (snd >> modPanel games) // Put the result in a modPanel
               ]) ]

let private modInfo (model: Model) (dispatch: Msg -> unit): IView =
    let mutable games = Map.empty

    stackPanelCls
        "modInfo"
        [ for (group, infos) in groupByState model.ModInfo |> Seq.sortBy fst do
            let groupExpander content =
                seq {
                    let cs, header =
                        groupHeader group infos
                        |> Option.defaultValue ([ "adornRight" ], textBlock [] "")

                    yield
                        expander
                            [ classes cs
                              isExpanded true
                              Expander.header header ]
                            // Output the file info
                            content
                }

            yield!
                match group with
                | FoundGroup ->
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
                                 |> Array.find (fun g -> g.Id = gameId))
                                    .Name

                            games <- games.Add(gameId, name)
                            name)
                    |> Seq.sortBy fst
                    |> Seq.map (gamePanel model.Games)
                | NotFoundGroup ->
                    let length = Seq.length infos

                    let rowDefs =
                        Array.create (3 * length - 1) "*"
                        |> String.concat ","
                        |> toRowDefinitions

                    groupExpander
                        (grid [ cls "modErrors"
                                toColumnDefinitions "*,auto"
                                rowDefs ] [
                            yield!
                                (Seq.sortBy orderBy infos
                                 |> Seq.mapi (fun i mi ->
                                     let j = 3 * i

                                     seq {
                                         modName [ cls "modName"; rowSpan 2; row j ] mi
                                         textCls "header" 1 j "MD5 Hash"
                                         textCls "md5sum" 1 (j + 1) (mi.Hash.ToUpper())

                                         if i < length - 1
                                         then textBlock [ cls "spacer"; row (j + 2) ] ""
                                     })
                                 |> Seq.concat)
                         ])

                | WorkingGroup ->
                    groupExpander
                        (stackPanelCls
                            "n"
                             (Seq.sortBy orderBy infos
                              |> Seq.map (fun mi -> view mi (fun msg -> ModInfoMsg(mi.Id, msg) |> dispatch))
                              |> Seq.toList)) ]

type ModChecker() =
    interface IPlugin with
        member __.Name = name

        member __.Description =
            "Verifies mod archive integrity with Nexus "

        member __.DarkStyle = "avares://ModChecker/ModChecker.xaml"
        member __.LightStyle = "avares://ModChecker/ModChecker.xaml"

        member __.Init(window, nexus, initialProperties, throttleUpdates) =
            Plugin.mapInit init (window, nexus, initialProperties, throttleUpdates)

        member __.Update(msg, model) = Plugin.mapUpdate update (msg, model)

        member __.View(model, dispatch) =
            Plugin.mapView ModChecker.TheView (model, dispatch)

    static member private TheView (model: Model) (dispatch: Dispatch<Msg>): IView =
        dockPanel [ cls "modChecker" ] [
            if model.Games.Length = 0 then
                yield!
                    [ progressBar [ dock Dock.Top
                                    isIndeterminate true ]

                      // Let's the progress bar take it's natural height and fills the rest with nothing
                      textBlock [] "" ]
            elif not <| model.IsGameSelected then
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
