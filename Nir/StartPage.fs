namespace Nir

open Avalonia.FuncUI.DSL

module StartPage =
    open Elmish
    open Avalonia.Controls
    open Nir.Controls
    open Avalonia.Layout

    type State =
        { Window: Window
          File: string option }

    let init window =
        { Window = window
          File = None },
        Cmd.none

    type Msg =
        | OpenLocalModList
        | AfterSelectFile of string[]

    let update (msg: Msg) (state: State) =
        match msg with
        | OpenLocalModList ->
            let dialog = Dialogs.getHtmlFileDialog None
            let showDialog window = dialog.ShowAsync(window) |> Async.AwaitTask
            state, Cmd.OfAsync.perform showDialog state.Window AfterSelectFile
        | AfterSelectFile files ->
            { state with File = (Array.tryExactlyOne files) },
            Cmd.none
        
    let view (_: State) (dispatch: Msg -> unit) =
        Grid.create [
            Grid.margin 10.0
            Grid.rowDefinitions "auto, auto, *, auto"
            Grid.children [
                textBlock 0 "subtitle" "Nir lets you install Skyrim Mod Guides from the Web"
                Button.create [
                    Grid.row 1
                    Button.content (TextBlock.create [
                        TextBlock.classes [ "subtitle" ]
                        TextBlock.text "Open a local HTML modlist" ] )
                    Button.onClick (fun _ -> dispatch OpenLocalModList) ]
                textBlock 2 "" ""
                textBlockEx 3 "link" "Advanced..." [
                    TextBlock.horizontalAlignment HorizontalAlignment.Right
                    TextBlock.verticalAlignment VerticalAlignment.Stretch ] ] ]
