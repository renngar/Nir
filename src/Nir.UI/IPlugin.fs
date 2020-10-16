namespace Nir.UI

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Nir.NexusApi
open Nir.Utility.INI

module Plugin =
    type Msg = obj

    type Model = obj

    type ThrottleUpdates = unit -> bool

    type ExternalMsg =
        | NoOp
        | SaveProperties of Properties

    type Init<'Model, 'Msg> = Window -> Nexus -> Properties -> ThrottleUpdates -> ('Model * Cmd<'Msg>)

    /// Map an init function to the `IPlugin.Init` signature
    let mapInit (init: Init<'Model, 'Msg>)
                (window: Window, nexus: Nexus, initialProperties: Properties, throttleUpdates: ThrottleUpdates)
                =
        init window nexus initialProperties throttleUpdates
        |> fun (model, cmd) -> model :> Model, Cmd.map (fun msg -> msg :> Msg) cmd

    /// Map an update function to the `IPlugin.Update` signature
    let mapUpdate (update: 'Msg -> 'Model -> ('Model * Cmd<'Msg> * ExternalMsg)) (msg: Msg, model: Model) =
        update (msg :?> 'Msg) (model :?> 'Model)
        |> fun (model, cmd, extMsg) -> model :> Model, Cmd.map (fun x -> x :> Msg) cmd, extMsg

    /// Map a view function to the `IPlugin.View` signature
    let mapView (view: 'Model -> Dispatch<'Msg> -> IView) (model: Model, dispatch: Dispatch<Model>) =
        view (model :?> 'Model) dispatch

/// The Interface all Nir plugins must implement
type IPlugin =
    abstract Name: string
    abstract Description: string
    abstract DarkStyle: string
    abstract LightStyle: string

    abstract Init: Window * Nexus * Properties * Plugin.ThrottleUpdates -> Plugin.Model * Cmd<Plugin.Msg>

    abstract Update: Plugin.Msg * Plugin.Model -> Plugin.Model * Cmd<Plugin.Msg> * Plugin.ExternalMsg
    abstract View: Plugin.Model * Dispatch<Plugin.Msg> -> IView
