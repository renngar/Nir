module Nir.Pages.Error

open Elmish
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Nir.UI.Controls

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
      Buttons = buttons },
    Cmd.none

// Update
type ExternalMsg = ButtonType

type Msg = ButtonClicked of ButtonType

let update msg model: Model * Cmd<_> * ExternalMsg =
    match msg with
    | ButtonClicked button -> model, Cmd.none, button

// View

let textBlock cls contents =
    TextBlock.textBlock
        [ classes [ cls ]
          horizontalAlignment HorizontalAlignment.Center ]
        contents

let button button' isDefault' (dispatch: Dispatch<Msg>) =
    textButton
        [ if isDefault' then yield (classes [ "default" ])
          yield!
              [ isDefault isDefault'
                onClick (fun _ -> dispatch (ButtonClicked button')) ] ]
        (match button' with
         | Retry -> "Retry"
         | Cancel -> "Cancel")

let getButtons model dispatch: IView list =
    match model.Buttons with
    | RetryCancel ->
        [ button Retry true dispatch
          button Cancel false dispatch ]

let view (model: Model) (dispatch: Msg -> unit): IView =
    stackPanel [ horizontalAlignment HorizontalAlignment.Center
                 margin 10.0
                 spacing 4.0 ] [
        textBlock "error" model.Title
        textBlock "h2" model.Message
        stackPanel
            [ orientation Orientation.Horizontal
              marginTopBottom 16.0
              spacing 16.0
              horizontalAlignment HorizontalAlignment.Center ]
            (getButtons model dispatch)
    ]
