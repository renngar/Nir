module Nir.Utility.Md5sum

open System.IO
open System.Security.Cryptography

let md5sum (fileName: string) onProgress =
    let blockSize = 1024 * 1024
    let info = FileInfo(fileName)
    use sr = new StreamReader(fileName)
    use br = new BinaryReader(sr.BaseStream)

    use md5hash =
        IncrementalHash.CreateHash(HashAlgorithmName.MD5)

    let mutable atEnd = false
    let mutable processed = 0L
    onProgress (processed, info.Length)

    while not atEnd do
        let block = br.ReadBytes(blockSize)
        md5hash.AppendData(block)
        processed <- processed + (int64 block.Length)
        onProgress (processed, info.Length)
        if block.Length < blockSize then atEnd <- true

    md5hash.GetHashAndReset()
    |> Seq.map (fun b -> sprintf "%02x" b)
    |> String.concat ""
