namespace Nir

open System
open Avalonia
open Avalonia.Platform

module Assets =
    /// Opens a stream for an asset inside of "Nir/Assets"
    let Open asset =
        let loader =
            AvaloniaLocator.Current.GetService<IAssetLoader>()

        loader.Open(Uri("avares://Nir/Assets/" + asset))
