[<AutoOpen>]
module Nir.UI.Controls.PageHeader

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open Nir.UI

let pageHeader title description =
    let theme =
        AvaloniaLocator.Current.GetService<IThemeSwitcher>()

    stackPanel [ dock Dock.Top // In case this is placed in a DockPanel
                 cls "pageHeader"
                 orientation Orientation.Vertical ] [
        yield
            grid [ toColumnDefinitions "*,*" ] [
                textBlockCls "h1" title
                Button.create [ column 1
                                horizontalAlignment HorizontalAlignment.Right
                                onClick (fun _ -> theme.Toggle())
                                Button.content
                                    (DrawingPresenter.create [ cls <| if theme.IsLight then "light" else "dark" ]) ]
            ]

        if description <> "" then
            yield
                description
                |> textBlock [ cls "description"
                               dock Dock.Top
                               TextBlock.textWrapping TextWrapping.Wrap ]
    ]
