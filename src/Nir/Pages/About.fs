module Nir.Pages.About

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Nir.UI
open Nir.UI.Controls
open Nir.Web


type Model =
    { NoOp: bool }
    interface IPageModel with
        member this.Title = "Thank you for using Avalonia.FuncUI"
        member this.Description = ""

type Links =
    | AvaloniaRepository
    | AvaloniaAwesome
    | AvaloniaGitter
    | AvaloniaCommunity
    | FuncUIRepository
    | FuncUIGitter
    | FuncUINetTemplates
    | FuncUISamples

type Msg = OpenUrl of Links

type ExternalMsg = | NoOp

let init = { NoOp = false }, Cmd.none

let update (msg: Msg) (state: Model) =
    match msg with
    | OpenUrl link ->
        match link with
        | AvaloniaRepository -> "https://github.com/AvaloniaUI/Avalonia"
        | AvaloniaAwesome -> "https://github.com/AvaloniaCommunity/awesome-avalonia"
        | AvaloniaGitter -> "https://gitter.im/AvaloniaUI"
        | AvaloniaCommunity -> "https://github.com/AvaloniaCommunity"
        | FuncUIRepository -> "https://github.com/AvaloniaCommunity/Avalonia.FuncUI"
        | FuncUIGitter -> "https://gitter.im/Avalonia-FuncUI"
        | FuncUINetTemplates -> "https://github.com/AvaloniaCommunity/Avalonia.FuncUI.ProjectTemplates"
        | FuncUISamples -> "https://github.com/AvaloniaCommunity/Avalonia.FuncUI/tree/master/src/Examples"
        |> openUrl

        state, Cmd.none, NoOp

let linkView (dockingPosition: Dock) dispatch header links =
    let link url text =
        textBlock
            [ cls "link"
              onTapped (fun _ -> dispatch (OpenUrl url)) ]
            text

    stackPanel [ dock dockingPosition
                 horizontalAlignment
                     (if dockingPosition = Dock.Left then HorizontalAlignment.Left else HorizontalAlignment.Right) ] [
        yield textBlockCls "h1" header
        yield! Seq.map (fun (url, text) -> link url text) links
    ]

let avaloniaLinksView (dock: Dock) (dispatch: Msg -> unit): IView =
    linkView
        dock
        dispatch
        "Avalonia"
        [ (AvaloniaRepository, "Avalonia Repository")
          (AvaloniaAwesome, "Awesome Avalonia")
          (AvaloniaGitter, "Gitter")
          (AvaloniaCommunity, "Avalonia Community") ]

let avaloniaFuncUILinksView (dock: Dock) (dispatch: Msg -> unit): IView =
    linkView
        dock
        dispatch
        "Avalonia.FuncUI"
        [ (FuncUIRepository, "Avalonia.FuncUI Repository")
          (FuncUIGitter, "Gitter")
          (FuncUINetTemplates, ".Net Templates")
          (FuncUISamples, "Samples") ]

let view (_: Model) (dispatch: Msg -> unit) =
    dockPanel [ cls "about"
                horizontalAlignment HorizontalAlignment.Left
                verticalAlignment VerticalAlignment.Top ] [
        textBlock
            [ dock Dock.Top; cls "subtitle" ]
            ("Avalonia.FuncUI is a project that provides you with an Elmish DSL for Avalonia Controls\n"
             + "for you to use in an F# idiomatic way. We hope you like the project and spread the word :)\n"
             + "Questions ? Reach to us on Gitter, also check the links below")
        avaloniaLinksView Dock.Left dispatch
        avaloniaFuncUILinksView Dock.Right dispatch
    ]
