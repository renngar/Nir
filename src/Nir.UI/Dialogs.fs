// System dialog interfaces.
//
// Copyright (C) 2020 Renngar <renngar@renngar.com>
//
// This file is part of Nir.
//
// Nir is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <https://www.gnu.org/licenses/>.
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
