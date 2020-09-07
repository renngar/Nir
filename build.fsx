open Fake.DotNet
#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"
open System.IO
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

let solution =  Path.Combine(Directory.GetCurrentDirectory(), "Nir.sln")

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
    DotNet.exec id "fsharplint" 
    <| "-f msbuild lint --lint-config fsharplint.json " + solution
    |> ignore
)

Target.create "Test" (fun _ ->
    DotNet.test id solution
    |> ignore
)

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Lint"
  ==> "Test"
  ==> "All"

Target.runOrDefault "Build"
