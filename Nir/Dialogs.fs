module Nir.Dialogs

open System
open System.Collections.Generic

open Avalonia.Controls

let promptHtmlFileName window = async {
    let filters =
        let filter = FileDialogFilter()
        filter.Extensions <- List([ "html"; "htm" ])
        filter.Name <- "HTML Mod List"
        seq { filter }

    let dialog = OpenFileDialog()
    dialog.Directory <-
        Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
    dialog.Title <- "Select a Mod List"
    dialog.Filters <- List(filters)

    let! files = dialog.ShowAsync(window) |> Async.AwaitTask
    return files.[0]
}

let promptModArchive window = async {
    let filters =
        let filter = FileDialogFilter()
        filter.Extensions <- List([ "7z"; "rar"; "zip" ])
        filter.Name <- "Mod Archive"
        seq { filter }

    let dialog = OpenFileDialog()
    dialog.Directory <-
        Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
    dialog.Title <- "Select a Mod Archive"
    dialog.Filters <- List(filters)

    return! dialog.ShowAsync(window) |> Async.AwaitTask
}
