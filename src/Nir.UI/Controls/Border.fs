[<AutoOpen>]

module Nir.UI.Controls.Border

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let border attributes (child: IView) =
    Border.create [ Border.child child
                    yield! attributes ] :> IView
