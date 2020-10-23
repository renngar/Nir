namespace Nir

open System

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI
open Avalonia.Platform

/// This is your application you can ose the initialize method to load styles
/// or handle Life Cycle events of your application
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Nir/Styles/LightTheme.xaml"
        this.Name <- "Nir"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let assets =
                AvaloniaLocator.Current.GetService<IAssetLoader>()

            let window = Shell.MainWindow()
            window.Icon <- WindowIcon(assets.Open(Uri("avares://Nir/Assets/Icons/Nir.ico")))
            desktopLifetime.MainWindow <- window
        | _ -> ()

module Program =

    [<STAThread>] // Required for Drag and drop from an external source
    [<EntryPoint>]
    let main (args: string []) =
        AppBuilder.Configure<App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime(args)
