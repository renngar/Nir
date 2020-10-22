[<AutoOpen>]
module Nir.UI.Controls.PageHeader

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open Nir.UI
open Nir.UI.Material

let pageHeader title description =
    let theme =
        AvaloniaLocator.Current.GetService<IThemeSwitcher>()

    stackPanel [ dock Dock.Top // In case this is placed in a DockPanel
                 cls "pageHeader"
                 orientation Orientation.Vertical ] [
        yield
            grid [ toColumnDefinitions "*,*" ] [
                textBlockCls "h1" title
                stackPanel [ cls "headerButtons"
                             column 1
                             horizontalAlignment HorizontalAlignment.Right
                             orientation Orientation.Horizontal ] [
                    textButton
                        [ cls "material"
                          onClick (fun _ -> theme.Toggle()) ]
                        (if theme.IsLight then Icons.wbSunny else Icons.nightsStay)
                    menu [ horizontalAlignment HorizontalAlignment.Right ] [
                        menuItem [ cls "material"; header Icons.menu ] [
                            menuItem [ horizontalAlignment HorizontalAlignment.Right
                                       header "About Nir Tools..." ] []
                        ]
                    ]
                ]
            ]

        if description <> "" then
            yield
                description
                |> textBlock [ cls "description"
                               dock Dock.Top
                               TextBlock.textWrapping TextWrapping.Wrap ]
    ]
