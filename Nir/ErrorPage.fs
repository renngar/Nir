module Nir.ErrorPage

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout

type Buttons = | RetryCancel

// Model

type Model =
    { Title: string
      Message: string
      Buttons: Buttons }

let init title message buttons =
    { Title = title
      Message = message
      Buttons = buttons }, Cmd.none

// Update

type Msg = Done of string

let update (_: Msg) (model: Model): Model * Cmd<_> = model, Cmd.none

// View

let textBlock cls text =
    TextBlock.create
        [ TextBlock.classes [ cls ]
          TextBlock.horizontalAlignment HorizontalAlignment.Center
          TextBlock.text text ]

let button (text: string) isDefault (dispatch: Msg -> unit): IView<Button> =
    Button.create
        [ Button.isDefault isDefault
          Button.content text
          Button.onClick (fun _ -> dispatch (Done text)) ]


let getButtons model dispatch: IView list =
    match model.Buttons with
    | RetryCancel ->
        [ button "Retry" true dispatch
          button "Cancel" false dispatch ]

let view (model: Model) (dispatch: Msg -> unit) =
    DockPanel.create
        [ DockPanel.children
            [ StackPanel.create
                [ StackPanel.horizontalAlignment HorizontalAlignment.Center
                  StackPanel.children
                      [ textBlock "error" model.Title
                        textBlock "subtitle" model.Message
                        StackPanel.create
                            [ StackPanel.orientation Orientation.Horizontal
                              StackPanel.horizontalAlignment HorizontalAlignment.Center
                              StackPanel.children (getButtons model dispatch) ] ] ] ] ]
