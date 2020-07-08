namespace Nir

module StartPage =
    open Elmish
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Components
    open Avalonia.FuncUI.Elmish

    type State =
        // fsharplint:disable-next-line RecordFieldNames
        { noop: bool }

    let init = { noop = false }

    type Msg = None

    let update (_: Msg) (state: State) =
        state

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

    type Host() as this =
        inherit Hosts.HostControl()
        do
            /// You can use `.mkProgram` to pass Commands around
            /// if you decide to use it, you have to also return a Command in the initFn
            /// (init, Cmd.none)
            /// you can learn more at https://elmish.github.io/elmish/basics.html
            let startFn () =
                init
            Program.mkSimple startFn update view
            |> Program.withHost this
            |> Program.run
        