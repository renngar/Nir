module Nir.Utility.Md5sum

open System.IO
open System.Security.Cryptography

let md5sum (fileName: string) =
    use sr = new StreamReader(fileName)
    use md5hash = MD5.Create()
    md5hash.ComputeHash(sr.BaseStream)
    |> Seq.map (fun b -> sprintf "%02x" b)
    |> String.concat ""
