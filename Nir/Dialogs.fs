namespace Nir

module Dialogs =
    open System
    open System.Collections.Generic
    open Avalonia.Controls

    let getHtmlFileDialog (filters: seq<FileDialogFilter> option) =
        let dialog = OpenFileDialog()
        
        let filters =
            match filters with
            | Some filter -> filter
            | None ->
                let filter = FileDialogFilter()
                filter.Extensions <- List(["html"; "htm"])
                filter.Name <- "HTML Mod List"
                seq{ filter }
                
        dialog.Directory <- Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
        dialog.Title <- "Select a Mod List"
        dialog.Filters <- List(filters)
        dialog