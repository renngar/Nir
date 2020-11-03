namespace Nir.UI

open Elmish
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Nir.NexusApi
open Nir.Utility.INI

type IPluginModel =
    abstract Title: string
    abstract Description: string

type IPluginMsg =
    interface
    end

type PluginDispatch< ^Msg when ^Msg :> IPluginMsg> = Dispatch< ^Msg >

module Plugin =
    type Msg = Msg of IPluginMsg

    type Model = Model of IPluginModel

    type ThrottleUpdates = unit -> bool

    type ExternalMsg =
        | NoOp
        | SaveProperties of Properties

    type Init<'Model, 'Msg> = Window -> Nexus -> Properties -> ThrottleUpdates -> ('Model * Cmd<'Msg>)

    /// Cast an abstract `'Msg` to a concrete `Msg` without triggering FSharpLint warnings.
    let inline private toMsg (msg: 'Msg) = Msg msg

    /// Map an init function to the `IPlugin.Init` signature
    let mapInit (init: Init<'Model, 'Msg>)
                (window: Window, nexus: Nexus, initialProperties: Properties, throttleUpdates: ThrottleUpdates)
                =
        init window nexus initialProperties throttleUpdates
        |> fun (model, cmd) -> Model model, Cmd.map toMsg cmd

    /// Map an update function to the `IPlugin.Update` signature
    let mapUpdate (update: 'Msg -> 'Model -> ('Model * Cmd<'Msg> * ExternalMsg)) ((Msg msg), (Model model)) =
        update (msg :?> 'Msg) (model :?> 'Model)
        |> fun (model, cmd, extMsg) -> Model model, Cmd.map toMsg cmd, extMsg

    /// Map a view function to the `IPlugin.View` signature
    let mapView (view: 'Model -> Dispatch<'Msg> -> IView) ((Model model), dispatch: Dispatch<Msg>) =
        view (model :?> 'Model) (toMsg >> dispatch)

/// The Interface all Nir plugins must implement
type IPlugin =
    abstract Name: string
    abstract Description: string
    abstract DarkStyle: string
    abstract LightStyle: string

    abstract Init: Window * Nexus * Properties * Plugin.ThrottleUpdates -> Plugin.Model * Cmd<Plugin.Msg>

    abstract Update: Plugin.Msg * Plugin.Model -> Plugin.Model * Cmd<Plugin.Msg> * Plugin.ExternalMsg
    abstract View: Plugin.Model * Dispatch<Plugin.Msg> -> IView
