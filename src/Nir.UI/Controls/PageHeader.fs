[<AutoOpen>]
module Nir.UI.Controls.PageHeader

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media


let pageHeader title description =
    stackPanel [ dock Dock.Top // In case this is placed in a DockPanel
                 cls "pageHeader"
                 orientation Orientation.Vertical ] [
        yield textBlockCls "h1" title
        if description <> "" then
            yield
                description
                |> textBlock [ cls "description"
                               dock Dock.Top
                               TextBlock.textWrapping TextWrapping.Wrap ]
    ]
