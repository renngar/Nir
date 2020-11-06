[<AutoOpen>]
module Nir.UI.Controls.MenuItem

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// Create a `MenuItem`.
let menuItem (attributes: seq<IAttr<MenuItem>>) (items: IView list): IView =
    MenuItem.create [ if not <| items.IsEmpty then yield MenuItem.viewItems items
                      yield! attributes ]
    |> Helpers.generalize

let header (str: string) = MenuItem.header str
