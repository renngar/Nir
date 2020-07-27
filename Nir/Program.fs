namespace Nir

open System

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI

open Nir.Utility.Path

/// This is your application you can ose the initialize method to load styles
/// or handle Life Cycle events of your application
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Nir/Styles/Styles.xaml"

        this.Name <- "Nir"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- Shell.MainWindow()
        | _ -> ()

module Program =

    [<STAThread>] // Required for Drag and drop from an external source
    [<EntryPoint>]
    let main (args: string []) =
        getProgramPath() |> printfn "%s"
        AppBuilder.Configure<App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime(args)
