[<AutoOpen>]
module Nir.UI.Controls.ToolTip

open Avalonia.Controls
open Avalonia.FuncUI.DSL

let toolTip = ToolTip.create

// Miscellaneous
let toTip (s: string) =
    Control.tip (toolTip [ ToolTip.content s ])
