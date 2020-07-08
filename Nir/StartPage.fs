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
        
    let view (_: State) (_: Msg -> unit) =
        DockPanel.create [
            DockPanel.horizontalAlignment HorizontalAlignment.Center
            DockPanel.verticalAlignment VerticalAlignment.Top
            DockPanel.children [
                TextBlock.create [
                    TextBlock.classes [ "title" ]
                    TextBlock.text "Nir" ] ] ]
        

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
        