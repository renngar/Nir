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
        let assemblyPath = _resolver.ResolveAssemblyToPath(assemblyName)
        if isNull assemblyPath then null else base.LoadFromAssemblyPath(assemblyPath)

    override __.LoadUnmanagedDll(unmanagedDllName) =
        let libraryPath = _resolver.ResolveUnmanagedDllToPath(unmanagedDllName)
        if isNull libraryPath
        then IntPtr.Zero
        else base.LoadUnmanagedDllFromPath(libraryPath)

let createCommands types =
    seq {
        for t in types do
            if typeof<IPlugin>.IsAssignableFrom(t) && not t.IsAbstract then
                let result = Activator.CreateInstance(t) :?> IPlugin
                yield result
    }

let loadPlugin (path: string) =
    try
        let loader = PluginLoader.CreateFromAssemblyFile(path, sharedTypes = [| typeof<IPlugin> |])
        loader.LoadDefaultAssembly().GetTypes() |> createCommands
    with _ -> Seq.empty

let findPlugins() =
    let nirProgramDirectory = typeof<PluginLoadContext>.Assembly.Location |> Path.GetDirectoryName
    let rxSep = string Path.DirectorySeparatorChar |> Regex.Escape
    let rxDev = sprintf "^(.*)%ssrc%sNir%s(bin%s.*)%snetcoreapp[0-9.]*$" rxSep rxSep rxSep rxSep rxSep
    let m = Regex.Match(nirProgramDirectory, rxDev)

    Path.Combine((if m.Success then m.Groups.[1].Value else nirProgramDirectory), "Plugins")
    |> Directory.EnumerateDirectories
    |> if m.Success
       then Seq.collect (fun d -> Directory.EnumerateDirectories(d +/ m.Groups.[2].Value))
       else id
    |> Seq.collect (fun d -> Directory.EnumerateFiles(d, "*.dll"))
    |> Seq.collect loadPlugin
    |> Seq.toArray
