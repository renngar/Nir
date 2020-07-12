namespace Nir

module Dialogs =
    open System
    open System.Collections.Generic
    open Avalonia.Controls

    let getHtmlFileDialog() =
        let filters =
            let filter = FileDialogFilter()
            filter.Extensions <- List(["html"; "htm"])
            filter.Name <- "HTML Mod List"
            seq { filter }

        let dialog = OpenFileDialog()
        dialog.Directory <- Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
        dialog.Title <- "Select a Mod List"
        dialog.Filters <- List(filters)
        dialog
