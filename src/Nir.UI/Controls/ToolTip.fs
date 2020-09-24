[<AutoOpen>]
module Nir.UI.Controls.toolTip

open Avalonia.Controls
open Avalonia.FuncUI.DSL

let toolTip = ToolTip.create

// Miscellaneous
let toTip (s: string) =
    Control.tip (toolTip [ ToolTip.content s ])
