[<AutoOpen>]
module Nir.UI.Controls.Attrs

open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL

let classes (classes: string list) = TemplatedControl.classes classes

let children = Grid.children

// Alignment settings
let horizontalAlignment = TemplatedControl.horizontalAlignment
let verticalAlignment = TemplatedControl.verticalAlignment

// Border  settings
let borderThicknessAll (x: float) = TemplatedControl.borderThickness x


// Dimension settings

/// Set the control width
let width = TemplatedControl.width

/// Set the control width
let height = TemplatedControl.height

/// Set the maximum control width
let maxWidth = TemplatedControl.maxWidth

/// Set the maximum control width
let maxHeight = TemplatedControl.maxHeight


// Docking settings
let dock = TemplatedControl.dock


// Font settings
let fontWeight = TextBlock.fontWeight
let fontFamily = TemplatedControl.fontFamily
let fontSize = TemplatedControl.fontSize
let fontStyle = TextBlock.fontStyle

// Margin settings

/// Set all the margins the same
let margin x = TemplatedControl.margin (x, x, x, x)

/// Set all the margins individually
let margins left top right bottom =
    TemplatedControl.margin (left, top, right, bottom)

/// Set the top and bottom margin
let marginTopBottom x = TemplatedControl.margin (0.0, x)

/// Set the side margins
let marginSides x = TemplatedControl.margin (0.0, x)

/// Set the left margin
let marginLeft x =
    TemplatedControl.margin (x, 0.0, 0.0, 0.0)

/// Set the top margin
let marginTop x =
    TemplatedControl.margin (0.0, x, 0.0, 0.0)

/// Set the right margin
let marginRight x =
    TemplatedControl.margin (0.0, 0.0, x, 0.0)

/// Set the bottom margin
let marginBottom x =
    TemplatedControl.margin (0.0, 0.0, 0.0, x)


// Padding settings

/// Set the padding
let padding left top right bottom =
    TemplatedControl.padding (left, top, right, bottom)

/// Set all the padding the same
let paddingAll x = TemplatedControl.padding (x, x, x, x)

/// Set the left padding
let paddingLeft x =
    TemplatedControl.padding (x, 0.0, 0.0, 0.0)

/// Set the top padding
let paddingTop x =
    TemplatedControl.padding (0.0, x, 0.0, 0.0)

/// Set the right padding
let paddingRight x =
    TemplatedControl.padding (0.0, 0.0, x, 0.0)

/// Set the bottom padding
let paddingBottom x =
    TemplatedControl.padding (0.0, 0.0, 0.0, x)

let isEnabled = TemplatedControl.isEnabled
