module Nir.Controls

open Avalonia.Controls
open Avalonia.FuncUI.DSL

let textBlock properties text = TextBlock.create <| List.append properties [ TextBlock.text text ]
let tbClasses (classes: string list) = TextBlock.classes classes
let tbClass cls = tbClasses [ cls ]
