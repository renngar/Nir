module ModChecker.ModInfo

open FSharp.Data
open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Media
open Nir.NexusApi
open Nir.UI
open Nir.UI.Controls
open Nir.Utility

type Msg =
    | CheckFile
    | MD5Progress of int64 * int64
    | MD5Complete of Result<string, string>
    | SearchResult of string list * ApiResult<Md5Search []>

type ArchiveState =
    | Starting
    | Hashing
    | Checking
    | Found of Md5Search []
    | NotFound of ApiError

type Model =
    { Id: int
      Nexus: Nexus
      SelectedGames: Game list
      ThrottleUpdates: Plugin.ThrottleUpdates
      Archive: string
      Hash: string
      State: ArchiveState
      ProgressCurrent: int64
      ProgressMax: int64 }

let orderBy modInfo =
    let baseName = Path.baseName modInfo.Archive

    match modInfo.State with
    | Found m -> 1, m.[0].Mod.Name, m.[0].Mod.GameId, baseName
    | NotFound _ -> 2, "", 0, baseName
    | Hashing
    | Checking -> 3, "", 0, baseName
    | Starting -> 4, "", 0, baseName

type StateOrder =
    | FoundGroup
    | NotFoundGroup
    | WorkingGroup
    | WaitingGroup

let groupByState =
    List.groupBy (fun modInfo ->
        match modInfo.State with
        | Found _ -> FoundGroup
        | NotFound _ -> NotFoundGroup
        | Hashing
        | Checking -> WorkingGroup
        | Starting -> WaitingGroup)

let groupHeader group (modInfos: Model list) =
    match group, modInfos.Head.State with
    | _, NotFound { StatusCode = code; Message = msg } ->
        match code with
        | HttpStatusCodes.NotFound -> "Unrecognized or Corrupted Archive "
        | _ -> msg
        |> textBlock [ classes [ "error" ] ]
        |> Some
    | WorkingGroup, _ ->
        textBlock [ classes [ "accent" ] ] "Processing"
        |> Some
    | _, _ -> None

let processingFile model =
    model.State = Hashing || model.State = Checking

let init nexus selectedGames throttleUpdates id file =
    { Id = id
      Nexus = nexus
      SelectedGames = selectedGames
      ThrottleUpdates = throttleUpdates
      Archive = file
      Hash = ""
      State = Starting
      ProgressCurrent = 0L
      ProgressMax = 0L },
    Cmd.ofMsg CheckFile

let private searchInDomains model gameDomains notFoundModel =
    let search model gameDomain remainingDomains =
        { model with State = Checking },
        Cmd.OfAsync.perform md5Search (model.Nexus, gameDomain, model.Hash) (fun result ->
            model.Id, SearchResult(remainingDomains, result))

    match Seq.toList gameDomains with
    | [ d ] -> search model d []
    | d :: rest -> search model d rest
    | [] -> notFoundModel, Cmd.none

let private checkHash model =
    let domains =
        Seq.map (fun (g: Game) -> g.DomainName) model.SelectedGames

    searchInDomains model domains model

let titleAndSub title subtitle =
    [ yield textBlock [ classes [ "h1" ]; dock Dock.Top ] title
      yield
          textBlock
              [ classes [ "h2" ]
                dock Dock.Top
                TextBlock.textWrapping TextWrapping.Wrap ]
              subtitle ]

module private Sub =
    let md5Search model onProgress onComplete dispatch =
        async {
            let dispatch' msg = (model.Id, msg) |> dispatch

            try
                Md5sum.md5sum model.Archive (fun x -> if model.ThrottleUpdates() |> not then onProgress x |> dispatch')
                |> Ok
            with e -> e.Message |> Error
            |> (onComplete >> dispatch')
        }
        |> Async.Start

let update msg (model: Model) =
    match msg with
    | CheckFile -> { model with State = Hashing }, Cmd.ofSub (Sub.md5Search model MD5Progress MD5Complete)
    | MD5Progress (current, max) ->
        { model with
              ProgressCurrent = current
              ProgressMax = max },
        Cmd.none
    | MD5Complete r ->
        match r with
        | Ok hash -> { model with Hash = hash } |> checkHash
        | Error e ->
            { model with
                  State = NotFound { StatusCode = -1; Message = e } },
            Cmd.none
    | SearchResult (gameDomains, r) ->
        match r with
        | Ok s ->
            { model with
                  Nexus = s.Nexus
                  State = Found s.Result },
            Cmd.none
        | Error e -> searchInDomains model gameDomains { model with State = NotFound e }

let view model (_: Dispatch<Msg>): IView =
    let modName model =
        textBlock [] (Path.baseName model.Archive)

    let stack content = [ stackPanel [ spacing 8.0 ] content ]

    let checking model =
        stack [ yield modName model
                if model.ProgressMax > 0L then
                    if model.State = Hashing then
                        yield
                            progressBar [ maximum (double model.ProgressMax)
                                          value (double model.ProgressCurrent) ]
                    else
                        yield progressBar [ isIndeterminate true ] ]

    dockPanel []
    <| match model.State with
       | Starting -> []
       | Hashing
       | Checking -> checking model
       | Found rs ->
           let r = rs.[0]

           [ yield! titleAndSub r.Mod.Name r.Mod.Summary
             yield textBlock [ fontWeight FontWeight.Bold ] r.FileDetails.Name ]
       | NotFound _ -> [ yield modName model ]
