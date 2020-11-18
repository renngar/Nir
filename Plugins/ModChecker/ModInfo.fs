module ModChecker.ModInfo

open FSharp.Data
open Elmish
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Nir.NexusApi
open Nir.UI
open Nir.UI.Controls
open Nir.Utility

type ArchiveState =
    | Hashing
    | Found of Md5Search []
    | NotFound of ApiError

type Model =
    { Id: int
      Nexus: Nexus
      SelectedGames: Game []
      ThrottleUpdates: Plugin.ThrottleUpdates
      Archive: string
      Hash: string
      State: ArchiveState
      ProgressCurrent: int64
      ProgressMax: int64 }

type Msg = ModInfoUpdate of Model

let orderBy modInfo =
    let baseName =
        (Path.baseName modInfo.Archive).ToLower()

    match modInfo.State with
    | Found m -> 1, m.[0].Mod.Name, m.[0].Mod.GameId, baseName
    | NotFound _ -> 2, "", 0, baseName
    | Hashing -> 3, "", 0, baseName

type StateOrder =
    | FoundGroup
    | NotFoundGroup
    | WorkingGroup

let groupByState (mi: Map<_, Model>) =
    mi
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.groupBy (fun modInfo ->
        match modInfo.State with
        | Found _ -> FoundGroup
        | NotFound _ -> NotFoundGroup
        | Hashing -> WorkingGroup)

let groupHeader group (modInfos: seq<Model>) =
    let text cls str =
        ([ "large"; cls ], textBlockCls cls str) |> Some

    match group, (Seq.head modInfos).State with
    | _, NotFound { StatusCode = code; Message = msg } ->
        match code with
        | HttpStatusCodes.NotFound -> "Unrecognized or Corrupted Archives"
        | _ -> msg
        |> text "error"
    | WorkingGroup, _ -> text "processing" "Processing"
    | _, _ -> None

module Sub =
    let private updateFile (id: int) (dispatch: int * Msg -> unit) (model: Model): unit =
        (id, ModInfoUpdate model) |> dispatch

    type private HashError = { ApiError: ApiError; Model: Model }

    let private lookupMod nexus selectedGames dispatch getHash model =
        async {
            let hashError: HashError ref =
                ref
                    { ApiError = { StatusCode = -1; Message = "" }
                      Model = model }

            let lookupHash model =
                let isOkOrSaveError model (result: ApiResult<Md5Search []>) =
                    match result with
                    | Ok _ -> true
                    | Error e ->
                        hashError := { ApiError = e; Model = model }
                        false

                model.SelectedGames
                |> Seq.map (fun (g: Game) ->
                    model.Nexus.Md5Search(g.DomainName, model.Hash)
                    |> Async.RunSynchronously)
                |> Seq.filter (isOkOrSaveError model)
                |> Seq.tryHead
                |> function
                | Some r ->
                    match r with
                    | Ok s ->
                        Some
                            { model with
                                  Nexus = nexus
                                  State = Found s }
                    | Error _ -> failwith "should never happen"
                | None -> None

            let update = updateFile model.Id dispatch

            // Do an initial update of the UI so it is more visually responsive to user actions.
            update model

            match model.State with
            | Found _ -> ()
            | _ ->
                let modelFromError () =
                    { hashError.Value.Model with
                          State = NotFound hashError.Value.ApiError }

                let newModel =
                    { model with
                          SelectedGames = selectedGames }

                update newModel

                try
                    newModel
                    |> getHash
                    |> (fun m ->
                        hashError := { hashError.Value with Model = m }
                        m)
                    |> lookupHash
                    |> function
                    | Some model -> model
                    | None -> modelFromError ()
                with _ -> modelFromError ()
                |> update
        }

    let processFile nexus selectedGames throttleUpdates id file dispatch =
        let reportProgress model (current, max) =
            if not <| model.ThrottleUpdates() then
                { model with
                      ProgressCurrent = current
                      ProgressMax = max }
                |> updateFile id dispatch

        { Id = id
          Nexus = nexus
          SelectedGames = selectedGames
          ThrottleUpdates = throttleUpdates
          Archive = file
          Hash = ""
          State = Hashing
          ProgressCurrent = 0L
          ProgressMax = 0L }
        |> lookupMod nexus selectedGames dispatch (fun model ->
               { model with
                     Hash = Md5sum.md5sum model.Archive (reportProgress model) })

    let reprocessFile (nexus: Nexus) (selectedGames: Game []) (model: Model) (dispatch: int * Msg -> unit): Async<unit> =
        lookupMod nexus selectedGames dispatch id model

let internal textCls cls c r text =
    textBlock
        [ classes [ "small"; cls ]
          row r
          column c ]
        text

let internal modName attributes model =
    textBlock attributes (Path.baseName model.Archive)

let view model (_: Dispatch<Msg>): IView =
    let checking (model: Model): IView list =
        [ stackPanelCls
            "checking"
              [ yield modName [] model
                yield
                    progressBar
                        // If we don't know how big the file is or have hashed it all and are waiting for a response
                        // from Nexus, display an indeterminate progress
                        (if model.ProgressMax = 0L
                            || model.ProgressCurrent = model.ProgressMax then
                            [ isIndeterminate true ]
                         else // otherwise, display how much has been hashed.
                             [ maximum (double model.ProgressMax)
                               value (double model.ProgressCurrent) ]) ] ]

    match model.State with
    | Hashing -> dockPanel [] (checking model)
    | Found _
    | NotFound _ -> failwith "Should be rendered in ModChecker"
