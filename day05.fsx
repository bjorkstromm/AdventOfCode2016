let getHash (doorId : string) (index : uint64) =
    let md5 = System.Security.Cryptography.MD5.Create()
    let bytes = System.Text.Encoding.ASCII.GetBytes(sprintf "%s%d" doorId index)
    md5.ComputeHash(bytes)
    |> Array.map (fun b -> b.ToString("x2"))
    |> Array.reduce (+)

let isMatch (hash : string) =
    hash.StartsWith("00000")

let getChar (hash : string) =
    match isMatch hash with
    | true -> Some hash.[5]
    | false -> None

let part1 doorId =
    let idx = seq { 0UL .. System.UInt64.MaxValue }

    idx
    |> Seq.map (getHash doorId)
    |> Seq.choose getChar
    |> Seq.take 8
    |> Seq.toArray
    |> System.String

// Part 2
let getChar2 (hash : string) =
    if isMatch hash then
        let idx = System.Char.GetNumericValue hash.[5] |> int
        if idx >= 0 && idx < 8 then
            Some (idx, hash.[6])
        else
            None
    else
        None

let part2 doorId =
    let idx = seq { 0UL .. System.UInt64.MaxValue }
    let ids = System.Collections.Generic.HashSet<int>()

    idx
    |> Seq.map (getHash doorId)
    |> Seq.choose getChar2
    |> Seq.filter (fun (idx, _) -> ids.Add idx)
    |> Seq.take 8
    |> Seq.sortBy fst
    |> Seq.map snd
    |> Seq.toArray
    |> System.String