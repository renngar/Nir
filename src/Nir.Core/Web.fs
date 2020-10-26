module Nir.Web

open System.Diagnostics
open System.Runtime.InteropServices

let openUrl url =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    then Process.Start(ProcessStartInfo("cmd", sprintf "/c start %s" url))
    elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
    then Process.Start("xdg-open", url)
    elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
    then Process.Start("open", url)
    else failwith "Unsupported OS Platform"
    |> ignore
