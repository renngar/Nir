module Nir.Pages.DownloadChecker

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
open Nir.NexusMods

type Msg =
    | FetchGames
    | GotGames of ApiResult<Game []>
    | GameSelected of int
    | OpenFileDialog
    | FileSelected of string
    | VerifyMods of seq<string>
    | MD5 of ApiResult<Md5Search []>

// Model

type Model =
    { Window: Window
      Nexus: Nexus
      Archive: string
      Games: Game []
      SelectedGames: Game list }

let init window nexus =
    { Window = window
      Nexus = nexus
      Archive = ""
      Games = [||]
      SelectedGames = [] }, Cmd.ofMsg FetchGames

// Update

let update (msg: Msg) (model: Model): Model * Cmd<_> =
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
    | GameSelected n -> { model with SelectedGames = [ model.Games.[n] ] }, Cmd.none
    | OpenFileDialog -> model, Cmd.OfAsync.perform promptModArchive model.Window FileSelected
    | VerifyMods fileNames -> model, Cmd.ofMsg (FileSelected(Seq.head fileNames))
    | FileSelected file ->
        { model with Archive = file },
        Cmd.OfAsync.perform md5Search (model.Nexus, model.SelectedGames.Head.DomainName, file) MD5
    | MD5 _ -> failwith "Not implemented"

// View

let view (model: Model) (dispatch: Msg -> unit) =
    let isGameSelected = not model.SelectedGames.IsEmpty

    let (contents: IView list) =
        [ TextBlock.create
            [ TextBlock.classes [ "h1" ]
              TextBlock.text "Nexus Download Checker" ]
          TextBlock.create
              [ TextBlock.classes [ "h2" ]
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.text
                    (if model.Games.Length = 0 then
                        "Fetching games from Nexus..."
                     elif isGameSelected then
                         "Drop mod archives below to verify they were correctly downloaded from Nexus"
                     else
                         "Select your game below") ]
          Grid.create
              [ Grid.columnDefinitions (if isGameSelected then "auto, *, auto" else "*")
                Grid.margin (0.0, 16.0)
                Grid.children
                    [ yield ComboBox.create
                                [ yield! [ Grid.column 0
                                           ComboBox.margin (0.0, 0.0, 8.0, 0.0)
                                           // ComboBox.width 180.0
                                           ComboBox.isEnabled (model.Games.Length > 0)
                                           ComboBox.dataItems model.Games
                                           ComboBox.itemTemplate
                                               (DataTemplateView<Game>
                                                   .create(fun data -> TextBlock.create [ TextBlock.text data.Name ]))
                                           ComboBox.onSelectedIndexChanged (GameSelected >> dispatch) ]
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
                                              |> VerifyMods
                                              |> dispatch)
                                      TextBox.textWrapping TextWrapping.Wrap
                                      TextBox.watermark "Mod archive to verify"
                                      // TextBox.height 30.0
                                      TextBox.verticalAlignment VerticalAlignment.Center
                                      TextBox.acceptsReturn false
                                      TextBox.acceptsTab false
                                      // This is tacky, but Ctrl-Insert does not work with Avalonia
                                      TextBox.tip (ToolTip.create [ ToolTip.content [ "Ctrl-V to paste" ] ])
                                      TextBox.text model.Archive
                                      TextBox.onTextChanged
                                          ((fun s -> seq { s })
                                           >> VerifyMods
                                           >> dispatch) ]
                          yield Button.create
                                    [ Grid.column 2
                                      Button.margin (8.0, 0.0, 0.0, 0.0)
                                      Button.isDefault true
                                      Button.classes [ "default" ]
                                      Button.onClick (fun _ -> dispatch OpenFileDialog)
                                      Button.content "Browse..." ] ] ] ]

    DockPanel.create
        [ DockPanel.children
            [ StackPanel.create
                [ StackPanel.margin 10.0
                  StackPanel.spacing 4.0
                  StackPanel.children contents ] ] ]
