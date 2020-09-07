﻿namespace Nir.UI

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Nir.NexusApi

module Plugin =
    type Msg = obj

    type Model = obj

    /// Map an init function to the `IPlugin.Init` signature
    let mapInit (init: Window -> Nexus -> ('Model * Cmd<'Msg>)) (window: Window, nexus: Nexus) =
        init window nexus |> fun (model, cmd) -> model :> Model, Cmd.map (fun msg -> msg :> Msg) cmd

    /// Map an update function to the `IPlugin.Update` signature
    let mapUpdate (update: 'Msg -> 'Model -> ('Model * Cmd<'Msg>)) (msg: Msg, model: Model) =
        update (msg :?> 'Msg) (model :?> 'Model)
        |> fun (model, cmd) -> model :> Model, Cmd.map (fun x -> x :> Msg) cmd

    /// Map a view function to the `IPlugin.View` signature
    let mapView (view: 'Model -> Dispatch<'Msg> -> IView) (model: Model, dispatch: Dispatch<Model>) =
        view (model :?> 'Model) dispatch

/// The Interface all Nir plugins must implement
type IPlugin =
    abstract Name: string
    abstract Description: string
    abstract Init: Window * Nexus -> Plugin.Model * Cmd<Plugin.Msg>
    abstract Update: Plugin.Msg * Plugin.Model -> Plugin.Model * Cmd<Plugin.Msg>
    abstract View: Plugin.Model * Dispatch<Plugin.Msg> -> IView
