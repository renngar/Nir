[<AutoOpen>]
module Nir.UI.Controls.Expander

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let expander attributes (content: IView) =
    Expander.create [ Expander.content content
                      yield! attributes ]
    |> Helpers.generalize

let isExpanded = Expander.isExpanded
