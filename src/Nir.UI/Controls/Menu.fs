[<AutoOpen>]
module Nir.UI.Controls.Menu

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

/// Create a menu
let menu attributes items =
    Menu.create [ Menu.viewItems items
                  yield! attributes ]
    |> Helpers.generalize

/// Creates a Menu with the given class and items
let menuCls ``class`` items = menu [ cls ``class`` ] items
