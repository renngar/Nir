module Nir.Pages.Error

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout

type ButtonType =
    | Retry
    | Cancel

type ButtonGroup = | RetryCancel

// Model

type Model =
    { Title: string
      Message: string
      Buttons: ButtonGroup }

let init title message buttons =
    { Title = title
      Message = message
      Buttons = buttons }, Cmd.none

// Update
type ExternalMsg = ButtonType

type Msg = ButtonClicked of ButtonType

let update msg model: Model * Cmd<_> * ExternalMsg =
    match msg with
    | ButtonClicked button -> model, Cmd.none, button

// View

let textBlock cls text =
    TextBlock.create
        [ TextBlock.classes [ cls ]
          TextBlock.horizontalAlignment HorizontalAlignment.Center
          TextBlock.text text ]

let button button' isDefault (dispatch: Msg -> unit): IView<Button> =
    Button.create
        [ if isDefault then yield (Button.classes [ "default" ])
          yield! [ Button.isDefault isDefault
                   Button.content
                       (match button' with
                        | Retry -> "Retry"
                        | Cancel -> "Cancel")
                   Button.onClick (fun _ -> dispatch (ButtonClicked button')) ] ]

let getButtons model dispatch: IView list =
    match model.Buttons with
    | RetryCancel ->
        [ button Retry true dispatch
          button Cancel false dispatch ]

let view (model: Model) (dispatch: Msg -> unit): IView =
    StackPanel.create
        [ StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.margin 10.0
          StackPanel.spacing 4.0
          StackPanel.children
              [ textBlock "error" model.Title
                textBlock "h2" model.Message
                StackPanel.create
                    [ StackPanel.orientation Orientation.Horizontal
                      StackPanel.margin (0.0, 16.0)
                      StackPanel.spacing 16.0
                      StackPanel.horizontalAlignment HorizontalAlignment.Center
                      StackPanel.children (getButtons model dispatch) ] ] ] :> IView
