module Nir.Dialogs

open System
open System.Collections.Generic

open Avalonia.Controls

type private FileQty =
    | Single
    | Multiple

let inline private openFileDialog (fileQty: FileQty) window title directory filters =
    let dialog = OpenFileDialog()
    dialog.Title <- title
    dialog.Directory <- directory
    dialog.Filters <- filters
    dialog.AllowMultiple <- (fileQty = Multiple)
    dialog.ShowAsync(window) |> Async.AwaitTask

/// Creates a FileDialogFilter
let inline private fileFilter name (extensions: string list) =
    FileDialogFilter(Name = name, Extensions = List extensions)

let promptHtmlFileName window =
    async {
        let! files =
            openFileDialog
                Single
                window
                "Select a Mod List"
                (Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments))
                (List [ fileFilter "HTML Mod List" [ "html"; "htm" ] ])

        return files.[0]
    }

let promptModArchives (window, directory) =
    openFileDialog
        Multiple
        window
        "Select Mod Archives"
        directory
        (List [ fileFilter "Mod Archive" [ "7z"; "rar"; "zip" ]
                fileFilter "All" [ "" ] ])
