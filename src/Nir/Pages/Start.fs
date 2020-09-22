module Nir.Pages.Start

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.Components
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open Nir.Controls
open Nir.Plugin
open Nir.UI

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
    ListBox.create [ ListBox.borderThickness 0.0
                     ListBox.padding 0.0
                     ListBox.onSelectedItemChanged (function
                         | :? IPlugin as p -> PluginSelected p |> dispatch
                         | _ -> ())
                     ListBox.dataItems model.Plugins
                     ListBox.itemTemplate
                         (DataTemplateView<IPlugin>
                             .create(fun p ->
                                    Border.create [ Border.classes [ "plugin" ]
                                                    Border.child
                                                        (StackPanel.create [ StackPanel.spacing 8.0
                                                                             StackPanel.children [ textBlock
                                                                                                       [ Class "h1" ]
                                                                                                       p.Name
                                                                                                   textBlock
                                                                                                       [ Class "h2" ]
                                                                                                       p.Description ] ]) ])) ]

// View
let view (model: Model) (dispatch: Msg -> unit): IView =
    StackPanel.create [ StackPanel.margin 10.0
                        StackPanel.spacing 4.0
                        StackPanel.children [ if not model.GotPlugins then
                                                  yield textBlock [] "Loading plugins..."

                                                  yield
                                                      ProgressBar.create [ ProgressBar.isIndeterminate true
                                                                           ProgressBar.margin (0.0, 16.0) ]
                                              elif model.Plugins.IsEmpty then
                                                  yield textBlock [] "No plugins found" :> IView
                                              else
                                                  yield textBlock [ Class "h1" ] "Tools"
                                                  yield pluginListView model dispatch ] ] :> IView
