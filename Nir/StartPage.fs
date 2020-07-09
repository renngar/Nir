namespace Nir

module StartPage =
    open Elmish
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.FuncUI.DSL

    type State =
        { Window: Window }

    let init window =
        { Window = window }, Cmd.none

    type Msg = None

    let update (_: Msg) (model: State) =
        model, Cmd.none

    // TODO: Move the textBlock... functions to a controls module
    let textBlockAttrs row cls text = [
        Grid.row row
        TextBlock.classes [ cls ]
        TextBlock.text text ]
        
    let textBlock row cls text =
        TextBlock.create
        <| textBlockAttrs row cls text 
    
    let textBlockEx row cls text rest  =
        TextBlock.create
        <| List.append (textBlockAttrs row cls text) rest
        
    let view (_: State) (_: Msg -> unit) =
        Grid.create [
            Grid.margin 10.0
            Grid.rowDefinitions "auto, auto, *, auto"
            Grid.children [
                textBlock 0 "title" "Nir"
                textBlock 1 "subtitle" "Nir lets you install Skyrim Mod Guides from the Web"
                textBlock 2 "" ""
                textBlockEx 3 "link" "Advanced..." [
                    TextBlock.horizontalAlignment HorizontalAlignment.Right
                    TextBlock.verticalAlignment VerticalAlignment.Stretch ] ] ]
        