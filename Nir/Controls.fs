module Nir.Controls

open Avalonia.Controls
open Avalonia.FuncUI.DSL

let textBlockAttrs row cls text =
    [ Grid.row row
      TextBlock.classes [ cls ]
      TextBlock.text text ]

let textBlock row cls text = TextBlock.create <| textBlockAttrs row cls text

let textBlockEx row cls text rest =
    TextBlock.create <| List.append (textBlockAttrs row cls text) rest
