module Nir.Pages.DownloadChecker

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input // DragDrop
open Avalonia.Layout
open Avalonia.Media

open Nir.Dialogs
open Nir.DSL // FuncUI DragDrop support
open Nir.NexusMods

// Model

type Model =
    { Window: Window
      ApiKey: string
      Archive: string }

let init window apiKey =
    { Window = window
      ApiKey = apiKey
      Archive = "" }, Cmd.none

// Update

type Msg =
    | OpenFileDialog
    | FileSelected of string
    | VerifyMods of seq<string>
    | MD5 of ApiResult<Md5Search []>

let update (msg: Msg) (model: Model): Model * Cmd<_> =
    match msg with
    | OpenFileDialog -> model, Cmd.OfAsync.perform promptModArchive model.Window FileSelected
    | VerifyMods fileNames -> model, Cmd.ofMsg (FileSelected(Seq.head fileNames))
    | FileSelected file ->
        { model with Archive = file }, Cmd.OfAsync.perform md5Search (model.ApiKey, "skyrim", file) MD5
    | MD5 md5sum -> failwith "Not implemented"

// View

let view (model: Model) (dispatch: Msg -> unit) =
    let mutable (contents: IView list) =
        [ TextBlock.create
            [ TextBlock.classes [ "h1" ]
              TextBlock.text "Nexus Download Checker" ]
          TextBlock.create
              [ TextBlock.classes [ "h2" ]
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.text "Drop mod archives below to verify they were correctly downloaded from Nexus" ]
          Grid.create
              [ Grid.columnDefinitions "*, auto"
                Grid.margin (0.0, 16.0)
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
                                  |> VerifyMods
                                  |> dispatch)
                          TextBox.textWrapping TextWrapping.Wrap
                          TextBox.watermark "Mod archive to verify"
                          TextBox.height 30.0
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
                      Button.create
                          [ Grid.column 1
                            Button.margin (16.0, 0.0, 0.0, 0.0)
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
