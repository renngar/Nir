module ModChecker.ModInfo

open FSharp.Data
open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Media
open Nir.NexusApi
open Nir.UI

type Msg =
    | CheckFile
    | MD5Progress of int64 * int64
    | MD5Complete of Result<string, string>
    | SearchResult of string list * ApiResult<Md5Search []>

type ArchiveState =
    | None
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

let processingFile model = model.State = Hashing || model.State = Checking

let init nexus selectedGames throttleUpdates id file =
    { Id = id
      Nexus = nexus
      SelectedGames = selectedGames
      ThrottleUpdates = throttleUpdates
      Archive = file
      Hash = ""
      State = None
      ProgressCurrent = 0L
      ProgressMax = 0L }, Cmd.ofMsg CheckFile

let private searchInDomains model gameDomains notFoundModel =
    let search model gameDomain remainingDomains =
        { model with State = Checking },
        Cmd.OfAsync.perform md5Search (model.Nexus, gameDomain, model.Hash)
            (fun result -> model.Id, SearchResult(remainingDomains, result))

    match Seq.toList gameDomains with
    | [ d ] -> search model d []
    | d :: rest -> search model d rest
    | [] -> notFoundModel, Cmd.none

let private checkHash model =
    let domains = Seq.map (fun (g: Game) -> g.DomainName) model.SelectedGames
    searchInDomains model domains model

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


module private Sub =
    let md5Search model onProgress onComplete dispatch =
        async {
            let dispatch' msg = (model.Id, msg) |> dispatch
            try
                Nir.Utility.Md5sum.md5sum model.Archive (onProgress >> dispatch') |> Ok
            with e -> e.Message |> Error
            |> (onComplete >> dispatch')
        }
        |> Async.Start

let update msg (model: Model) =
    match msg with
    | CheckFile -> { model with State = Hashing }, Cmd.ofSub (Sub.md5Search model MD5Progress MD5Complete)
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

let view model (_: Dispatch<Msg>): IView =
    let contents: IView list =
        match model.State with
        | None -> []
        | Hashing ->
            [ ProgressBar.create
                [ ProgressBar.maximum (double model.ProgressMax)
                  ProgressBar.value (double model.ProgressCurrent) ] ]
        | Checking -> [ ProgressBar.create [ ProgressBar.isIndeterminate true ] ]
        | Found rs ->
            let r = rs.[0]
            [ yield! titleAndSub r.Mod.Name r.Mod.Summary

              TextBlock.create
                  [ TextBlock.fontWeight FontWeight.Bold
                    TextBlock.text r.FileDetails.Name ] ]
        | NotFound { StatusCode = code; Message = msg } ->
            [ TextBlock.create
                [ TextBlock.classes [ "error" ]
                  TextBlock.text
                      (match code with
                       | HttpStatusCodes.NotFound -> "Unrecognized or Corrupted Archive"
                       | _ -> msg) ] ]
    DockPanel.create [ if not <| contents.IsEmpty then DockPanel.children contents ] :> IView
