module Nir.Utility.Md5sum

open System
open System.IO
open System.Security.Cryptography

let md5sum (fileName: string) onProgress =
    let blockSize = 1024 * 1024
    let info = FileInfo(fileName)
    use sr = new StreamReader(fileName)
    use br = new BinaryReader(sr.BaseStream)
    use md5hash = IncrementalHash.CreateHash(HashAlgorithmName.MD5)

    let mutable atEnd = false
    let mutable processed = 0L
    let mutable lastUpdate = DateTime.Now
    onProgress (processed, info.Length)
    while not atEnd do
        let block = br.ReadBytes(blockSize)
        md5hash.AppendData(block)
        processed <- processed + (int64 block.Length)
        if block.Length < blockSize then atEnd <- true
        else
            // This should really be handled by the framework, but we need to throttle the frequency of updates
            // otherwise the UI does not have time to redraw between them and no progress will be displayed which is
            // really bad for a large file that takes several seconds.
            let now = DateTime.Now
            let interval = now - lastUpdate
            if interval.Milliseconds > 50 then
                onProgress (processed, info.Length)
                lastUpdate <- now
    onProgress (processed, info.Length)

    md5hash.GetHashAndReset()
    |> Seq.map (fun b -> sprintf "%02x" b)
    |> String.concat ""