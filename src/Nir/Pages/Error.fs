module Nir.Pages.Error

open Elmish
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Nir.UI
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
    interface IPageModel with
        member this.HistoryStyle = Modal
        member this.Title = "Error"
        member this.Description = ""

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

let private textBlock (cls: string list) contents =
    TextBlock.textBlock
        [ classes cls
          horizontalAlignment HorizontalAlignment.Center ]
        contents

let private button button' isDefault' (dispatch: Dispatch<Msg>) =
    textButton
        [ isDefault isDefault'
          onClick (fun _ -> dispatch (ButtonClicked button')) ]
        (match button' with
         | Retry -> "Retry"
         | Cancel -> "Cancel")

let private getButtons model dispatch: IView list =
    match model.Buttons with
    | RetryCancel ->
        [ button Retry true dispatch
          button Cancel false dispatch ]

let view (model: Model) (dispatch: Msg -> unit): IView =
    stackPanel [ cls "error"
                 horizontalAlignment HorizontalAlignment.Center ] [
        textBlock [ "h1"; "error" ] model.Title
        textBlock [ "h2" ] model.Message
        stackPanel
            [ cls "buttons"
              orientation Orientation.Horizontal
              horizontalAlignment HorizontalAlignment.Center ]
            (getButtons model dispatch)
    ]
