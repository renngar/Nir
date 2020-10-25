[<AutoOpen>]
module Nir.UI.Controls.Menu

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// Create a menu
let menu attributes items =
    Menu.create [ Menu.viewItems items
                  yield! attributes ] :> IView

/// Creates a Menu with the given class and items
let menuCls ``class`` items = menu [ cls ``class`` ] items
