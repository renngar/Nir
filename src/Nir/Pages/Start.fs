module Nir.Pages.Start

open Avalonia.Controls
open Elmish
open Avalonia.FuncUI.Types

open Nir.Plugin
open Nir.UI
open Nir.UI.Controls

// Model
type Model =
    { Window: Window
      GotPlugins: bool
      Plugins: IPlugin list }

    member private this.TitleInfo =
        if not this.GotPlugins
        then ("Loading Plugins", "")
        elif this.Plugins.IsEmpty
        then ("", "No plugins found")
        else ("Tools", "Which tool would you like to run?")

    interface IPageModel with
        member this.Title = fst this.TitleInfo
        member this.Description = snd this.TitleInfo

type ExternalMsg =
    | NoOp
    | LaunchPlugin of IPlugin

type Msg =
    | GotPlugins of IPlugin []
    | PluginSelected of IPlugin

let init window =
    { Window = window
      GotPlugins = false
      Plugins = [] },
    Cmd.OfFunc.perform findPlugins () GotPlugins

let update msg model =
    match msg with
    | GotPlugins ps ->
        let plugins = Seq.toList ps

        let newModel =
            { model with
                  GotPlugins = true
                  Plugins = plugins }

        match plugins with
        // If there is only one plugin, launch it
        | [ plugin ] -> newModel, Cmd.none, LaunchPlugin plugin
        // Otherwise, do nothing and await manual selection
        | _ -> newModel, Cmd.none, NoOp
    | PluginSelected plugin -> model, Cmd.none, LaunchPlugin plugin

let private pluginListView (model: Model) (dispatch) =
    listBox [ onSelectedItemChanged (function
                  | :? IPlugin as p -> PluginSelected p |> dispatch
                  | _ -> ())
              dataItems model.Plugins
              itemTemplate (fun (p: IPlugin) ->
                  border [ cls "plugin" ]
                  <| stackPanel [] [
                      textBlock [ cls "h1" ] p.Name
                      textBlock [ cls "h2" ] p.Description
                     ]) ]

// View
let view (model: Model) (dispatch: Dispatch<Msg>): IView =
    stackPanelCls
        "start"
        [ if model.GotPlugins && not model.Plugins.IsEmpty
          then yield pluginListView model dispatch ]
