module Nir.Controls

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media

type TextBlockProperty =
    | Class of string
    | Classes of string list
    | GridRow of int
    | HorizontalAlignment of HorizontalAlignment
    | VerticalAlignment of VerticalAlignment
    | TextWrapping of TextWrapping

let private tbProperty =
    function
    | Class cls -> TextBlock.classes [ cls ]
    | Classes classes -> TextBlock.classes classes
    | TextWrapping wrapping -> TextBlock.textWrapping wrapping
    | GridRow n -> Grid.row n
    | HorizontalAlignment a -> TextBlock.horizontalAlignment a
    | VerticalAlignment a -> TextBlock.verticalAlignment a

let textBlock properties text =
    TextBlock.create
    <| List.append (List.map tbProperty properties) [ TextBlock.text text ]
