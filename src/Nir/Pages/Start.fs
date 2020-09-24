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

let pluginListView (model: Model) (dispatch) =
    listBox [ borderThicknessAll 0.0
              paddingAll 0.0
              onSelectedItemChanged (function
                  | :? IPlugin as p -> PluginSelected p |> dispatch
                  | _ -> ())
              dataItems model.Plugins
              itemTemplate (fun (p: IPlugin) ->
                  border [ classes [ "plugin" ] ]
                  <| stackPanel [ spacing 8.0 ] [
                      textBlock [ classes [ "h1" ] ] p.Name
                      textBlock [ classes [ "h2" ] ] p.Description
                     ]) ]

// View
let view (model: Model) (dispatch: Msg -> unit): IView =
    stackPanel [ margin 10.0; spacing 4.0 ] [
        if not model.GotPlugins then
            yield textBlock [] "Loading plugins..."

            yield
                progressBar [ isIndeterminate true
                              marginTopBottom 16.0 ]
        elif model.Plugins.IsEmpty then
            yield textBlock [] "No plugins found"
        else
            yield textBlock [ classes [ "h1" ] ] "Tools"
            yield pluginListView model dispatch
    ]
