[<AutoOpen>]
module Nir.UI.Controls.PageHeader

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open Nir.UI
open Nir.UI.Material

let pageHeader title description =
    let theme =
        AvaloniaLocator.Current.GetService<IThemeSwitcher>()

    let titleBlock = textBlockCls "h1" title

    let descriptionBlock =
        if description <> ""
        then [ description |> textBlock [ cls "description" ] ]
        else []

    let buttonsAndMenu =
        stackPanelCls
            "headerButtons"
            [ textButton
                [ cls "material"
                  onClick (fun _ -> theme.Toggle()) ]
                  (if theme.IsLight then Icons.wbSunny else Icons.nightsStay)
              menuCls
                  "more"
                  [ menuItem [ cls "material"
                               header Icons.moreVert ] [
                      menuItem [ header "About Nir Tools..." ] []
                    ] ] ]

    grid [ cls "pageHeader"
           toColumnDefinitions "*,*" ] [
        stackPanelCls "pageHeader" [ titleBlock; yield! descriptionBlock ]
        buttonsAndMenu
    ]
