[<AutoOpen>]
module Nir.UI.Controls.ScrollViewer

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let scrollViewer attributes (content: IView) =
    ScrollViewer.create [ ScrollViewer.content content
                          yield! attributes ] :> IView
