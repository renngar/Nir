#r "paket:
source https://api.nuget.org/v3/index.json
nuget FSharp.Core 4.7.2
nuget FSharp.Compiler.Service
nuget Fake
nuget Fake.Core.CommandLineParsing
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fantomas
nuget Fantomas.Extras //"
#load ".fake/build.fsx/intellisense.fsx"

open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Core.TargetOperators
open Fantomas
open Fantomas.Extras

let cli = """
usage: dotnet fake [options] command

options:
  -i           Ignore code format
  -f           Force copy Git hooks

Without '-f' Git hooks are only installed if they do not already exist.
"""

let sourcePath x = Path.Combine(__SOURCE_DIRECTORY__, x)
let solution = sourcePath "Nir.sln"

let dotnet cmd args = DotNet.exec id cmd args |> ignore

let srcFiles =
    !! "**/*.fs" ++ "**/*.fsx"
    -- "**/bin/**"
    -- "**/obj/**"
    -- "packages/**"
    -- ".fake/**"

let arguments =
    Target.getArguments ()
    |> Option.defaultValue Array.empty

let hasArg x = Seq.contains x arguments
let checkFormat = not <| hasArg "-i"
let forceCopyGitHooks = hasArg "-f"

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "Plugins/**/bin"
    ++ "Plugins/**/obj"
    |> Shell.cleanDirs)

Target.create "GitHooks" (fun _ ->
    !!(sourcePath ".githooks/*")
    |> Seq.iter (fun fileName ->
        let source = FileInfo(fileName)

        let target =
            sourcePath ".git" </> "hooks" </> source.Name

        if forceCopyGitHooks || not <| File.exists (target)
        then source.CopyTo(target, forceCopyGitHooks) |> ignore))

Target.create "Build" (fun _ -> DotNet.build id "Nir.sln")

// This is an independent target.  It is baked into build via Directory.Build.props, but it may come in handy for
// manual linting without building, it could be used by Emacs flycheck, if you don't have LSP or something similar.
Target.create "Lint" (fun _ ->
    dotnet
        "fsharplint"
        ("-f msbuild lint --lint-config fsharplint.json "
         + solution))

Target.create "Test" (fun _ -> DotNet.test id solution |> ignore)

Target.create "CheckCodeFormat" (fun _ ->
    let result =
        srcFiles
        |> FakeHelpers.checkCode
        |> Async.RunSynchronously

    if result.IsValid then
        Trace.log "No files need formatting"
    elif result.NeedsFormatting then
        Trace.log "The following files need formatting:"
        List.iter Trace.log result.Formatted
        failwith "Some files need formatting, check output for more info"
    else
        Trace.logf "Errors while formatting: %A" result.Errors)

// Manual target to autoformat the code.
Target.create "Format" (fun _ ->
    srcFiles
    |> FakeHelpers.formatCode
    |> Async.RunSynchronously
    |> printfn "Formatted files: %A")

Target.create "Rebuild" ignore
Target.create "All" ignore
Target.create "Check" ignore

let publishDir = @"..\..\Published"
let nirDir = publishDir </> "Nir"
let nirExe = nirDir </> "Nir.exe"

Target.create "Publish" (fun _ ->
    Directory.SetCurrentDirectory(sourcePath "src/Nir")

    let isWindows =
        RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

    let rid =
        if isWindows then "win-x86"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then "linux-x86"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then "osx-x64"
        else failwith "Unsupported OS platform"

    dotnet "publish" (sprintf @"-c Release --self-contained -r %s -o %s" rid nirDir)

    if isWindows then
        Process
            .Start(@"..\..\Tools\rcedit-x86.exe", sprintf @"%s --set-icon Assets\Icons\Nir.ico" nirExe)
            .WaitForExit()

    Directory.SetCurrentDirectory(sourcePath "Plugins/ModChecker")

    nirDir </> @"Plugins\ModChecker"
    |> sprintf @"-c Release -r %s -o %s" rid
    |> dotnet "publish")

Target.create "Package" (fun _ ->
    let nirVersion =
        FileVersionInfo
            .GetVersionInfo(nirExe)
            .ProductVersion
        |> fun s -> Regex.Replace(s, @"\+.*", "")

    !!(nirDir </> "**")
    |> Zip.filesAsSpecs nirDir
    |> Zip.moveToFolder "Nir"
    |> Zip.zipSpec (publishDir </> (sprintf "Nir-%s.zip" nirVersion)))

Target.create "Precommit" ignore

"Clean" ?=> "Build" ==> "Test"

"Clean" ==> "Rebuild"
"GitHooks" ==> "Build" ==> "Rebuild"

"CheckCodeFormat" ==> "Precommit"
"Lint" ==> "Precommit"
"Precommit" <=> "Check"

"CheckCodeFormat" =?> ("All", checkFormat)
"GitHooks" ==> "All"
"Rebuild" ==> "All"
"Lint" ==> "All"
"Test" ==> "All"

"All" <=> "Publish" ==> "Package"

Target.runOrDefaultWithArguments "Build"
