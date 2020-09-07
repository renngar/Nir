open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"

let getPath x = Path.Combine(__SOURCE_DIRECTORY__, x)
let solution = getPath "Nir.sln"

let dotnet cmd args = DotNet.exec id cmd args |> ignore

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "Plugins/**/bin"
    ++ "Plugins/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
    DotNet.build id "Nir.sln"
)

Target.create "Lint" (fun _ ->
    dotnet "fsharplint" ("-f msbuild lint --lint-config fsharplint.json " + solution)
)

Target.create "Test" (fun _ ->
    DotNet.test id solution
    |> ignore
)

Target.create "All" ignore

Target.create "Publish" (fun _ ->
    Directory.SetCurrentDirectory(getPath "src/Nir")
    let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    let rid = if isWindows then "win-x86"
              elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then "linux-x86"
              elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then "osx-x64"
              else failwith "Unsupported OS platform"
    dotnet "publish" (sprintf @"-c Release --self-contained -r %s -o ..\..\Published\Nir" rid)
    if isWindows then
        Process.Start(@"..\..\Tools\rcedit-x86.exe",
                      @"..\..\Published\Nir\Nir.exe --set-icon Assets\Icons\Nir.ico").WaitForExit()
    Directory.SetCurrentDirectory(getPath "Plugins/ModChecker")
    dotnet "publish" (sprintf @"-c Release -r %s -o ..\..\Published\Nir\Plugins\ModChecker" rid)
)

"Clean"
  ==> "Build"
  ==> "Lint"
  ==> "Test"
  ==> "All"

"All"
  ==> "Publish"

Target.runOrDefault "Build"
