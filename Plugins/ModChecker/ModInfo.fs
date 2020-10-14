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
      SelectedGames: Game list
      ThrottleUpdates: Plugin.ThrottleUpdates
      Archive: string
      Hash: string
      State: ArchiveState
      ProgressCurrent: int64
      ProgressMax: int64 }

type Msg = ModInfoUpdate of Model

let orderBy modInfo =
    let baseName = Path.baseName modInfo.Archive

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
    let text cls = textBlockCls cls >> Some

    match group, (Seq.head modInfos).State with
    | _, NotFound { StatusCode = code; Message = msg } ->
        match code with
        | HttpStatusCodes.NotFound -> "Unrecognized or Corrupted Archives"
        | _ -> msg
        |> text "error"
    | WorkingGroup, _ -> text "processing" "Processing"
    | _, _ -> None

module Sub =
    let processFile nexus selectedGames throttleUpdates id file dispatch =
        async {
            let updateFile model = (id, ModInfoUpdate model) |> dispatch

            let mutable apiError = { StatusCode = -1; Message = "" }

            let model =
                { Id = id
                  Nexus = nexus
                  SelectedGames = selectedGames
                  ThrottleUpdates = throttleUpdates
                  Archive = file
                  Hash = ""
                  State = Hashing
                  ProgressCurrent = 0L
                  ProgressMax = 0L }

            updateFile model

            try
                let isOkOrSaveError (result: ApiResult<Md5Search []>) =
                    match result with
                    | Ok _ -> true
                    | Error e ->
                        apiError <- e
                        false

                let reportProgress (current, max) =
                    if not <| model.ThrottleUpdates() then
                        { model with
                              ProgressCurrent = current
                              ProgressMax = max }
                        |> updateFile

                { model with
                      Hash = Md5sum.md5sum model.Archive reportProgress }
                |> fun model -> Seq.map (fun (g: Game) -> g.DomainName, model) model.SelectedGames
                |> Seq.map (fun (domain, model) ->
                    model.Nexus.md5Search (domain, model.Hash)
                    |> Async.RunSynchronously)
                |> Seq.filter isOkOrSaveError
                |> Seq.head
                |> fun r ->
                    match r with
                    | Ok s ->
                        { model with
                              Nexus = nexus
                              State = Found s }
                    | Error _ -> failwith "should never happen"
            with _ -> { model with State = NotFound apiError }
            |> updateFile
        }

let view model (_: Dispatch<Msg>): IView =
    let modName model =
        textBlock [] (Path.baseName model.Archive)

    let checking model =
        [ stackPanelCls
            "checking"
              [ yield modName model
                if model.ProgressMax > 0L then
                    yield
                        progressBar
                            (if (model.State <> Hashing)
                                || model.ProgressCurrent = model.ProgressMax then
                                [ isIndeterminate true ]
                             else
                                 [ maximum (double model.ProgressMax)
                                   value (double model.ProgressCurrent) ]) ] ]

    dockPanel []
    <| match model.State with
       | Hashing -> checking model
       | Found _ -> failwith "Should be rendered in ModChecker"
       | NotFound _ -> [ yield modName model ]
