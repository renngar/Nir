#r "paket:
source https://api.nuget.org/v3/index.json
nuget FSharp.Compiler.Service
nuget Fake
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fantomas
nuget Fantomas.Extras //"
#load ".fake/build.fsx/intellisense.fsx"

open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fantomas
open Fantomas.Extras

Target.initEnvironment ()

let getPath x = Path.Combine(__SOURCE_DIRECTORY__, x)
let solution = getPath "Nir.sln"

let dotnet cmd args = DotNet.exec id cmd args |> ignore

let srcFiles =
    !! "**/*.fs"
    ++ "**/*.fsx"
    -- "**/bin/**"
    -- "**/obj/**"
    -- "packages/**"
    -- ".fake/**"

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "Plugins/**/bin"
    ++ "Plugins/**/obj"
    |> Shell.cleanDirs)

Target.create "Build" (fun _ -> DotNet.build id "Nir.sln")

Target.create "BuildAll" (fun _ -> ())

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

Target.create "Format" (fun _ ->
    srcFiles
    |> FakeHelpers.formatCode
    |> Async.RunSynchronously
    |> printfn "Formatted files: %A")

Target.create "All" ignore

Target.create "Publish" (fun _ ->
    Directory.SetCurrentDirectory(getPath "src/Nir")

    let isWindows =
        RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

    let rid =
        if isWindows then "win-x86"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then "linux-x86"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then "osx-x64"
        else failwith "Unsupported OS platform"

    dotnet "publish" (sprintf @"-c Release --self-contained -r %s -o ..\..\Published\Nir" rid)

    if isWindows then
        Process.Start(@"..\..\Tools\rcedit-x86.exe", @"..\..\Published\Nir\Nir.exe --set-icon Assets\Icons\Nir.ico")
               .WaitForExit()

    Directory.SetCurrentDirectory(getPath "Plugins/ModChecker")
    dotnet "publish" (sprintf @"-c Release -r %s -o ..\..\Published\Nir\Plugins\ModChecker" rid))

"Clean" ==> "BuildAll"

"Build" ==> "BuildAll"

"BuildAll" ==> "Lint" ==> "Test" ==> "All"

"CheckCodeFormat" ==> "All"

"All" ==> "Publish"

Target.runOrDefault "Build"
