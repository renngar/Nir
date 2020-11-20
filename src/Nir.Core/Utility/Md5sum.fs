// MD5 routines.
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
