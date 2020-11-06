[<AutoOpen>]
module Nir.UI.Controls.Button

open Avalonia.FuncUI
open Avalonia.Controls
open Avalonia.FuncUI.DSL

/// Create a button
let textButton attributes (text: string) =
    Button.create [ Button.content text
                    yield! attributes ]
    |> Helpers.generalize

let isDefault = Button.isDefault

let onClick = Button.onClick
