// Plugin interface for Nir plugins.
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
module Nir.Plugin

open System
open System.IO
open System.Runtime.Loader
open System.Text.RegularExpressions
open McMaster.NETCore.Plugins
open Nir.UI
open Nir.Utility.Path

type PluginLoadContext(pluginPath: string) =
    inherit AssemblyLoadContext()

    let _resolver = AssemblyDependencyResolver pluginPath

    override __.Load(assemblyName) =
        let assemblyPath =
            _resolver.ResolveAssemblyToPath(assemblyName)

        if isNull assemblyPath then null else base.LoadFromAssemblyPath(assemblyPath)

    override __.LoadUnmanagedDll(unmanagedDllName) =
        let libraryPath =
            _resolver.ResolveUnmanagedDllToPath(unmanagedDllName)

        if isNull libraryPath
        then IntPtr.Zero
        else base.LoadUnmanagedDllFromPath(libraryPath)

let createCommands types =
    seq {
        for t in types do
            if typeof<IPlugin>.IsAssignableFrom(t)
               && not t.IsAbstract then
                let result = Activator.CreateInstance(t) :?> IPlugin
                yield result
    }

let loadPlugin (path: string) =
    try
        let loader =
            PluginLoader.CreateFromAssemblyFile(path, sharedTypes = [| typeof<IPlugin> |])

        loader.LoadDefaultAssembly().GetTypes()
        |> createCommands
    with _ -> Seq.empty

let findPlugins () =
    let nirProgramDirectory =
        typeof<PluginLoadContext>.Assembly.Location
        |> Path.GetDirectoryName

    let rxSep =
        string Path.DirectorySeparatorChar |> Regex.Escape

    let rxDev =
        sprintf "^(.*)%ssrc%sNir%s(bin%s.*)%snetcoreapp[0-9.]*$" rxSep rxSep rxSep rxSep rxSep

    let m = Regex.Match(nirProgramDirectory, rxDev)

    Path.Combine((if m.Success then m.Groups.[1].Value else nirProgramDirectory), "Plugins")
    |> Directory.EnumerateDirectories
    |> if m.Success
       then Seq.collect (fun d -> Directory.EnumerateDirectories(d +/ m.Groups.[2].Value))
       else id
    |> Seq.collect (fun d -> Directory.EnumerateFiles(d, "*.dll"))
    |> Seq.collect loadPlugin
    |> Seq.toArray
