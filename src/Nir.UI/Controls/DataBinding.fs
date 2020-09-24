[<AutoOpen>]
module Nir.UI.Controls.DataBinding

open Avalonia.Controls
open Avalonia.FuncUI.Components
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let dataItems = ListBox.dataItems

let dataTemplateView (view: ('T -> IView)) = DataTemplateView<'T>.create(view)

let itemTemplate x =
    ListBox.itemTemplate (dataTemplateView x)
