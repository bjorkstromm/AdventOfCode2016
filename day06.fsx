let matrix filepath =
    System.IO.File.ReadAllLines filepath
    |> Array.map (fun line -> line.ToCharArray())
    |> array2D

let mostFrequent xs =
    xs
    |> Seq.groupBy id
    |> Seq.map (fun (k, v) -> (k, Seq.length v))
    |> Seq.maxBy snd
    |> fst

let part1 filepath =
    let m = matrix filepath
    let len = m |> Array2D.length2

    seq { 0..len-1 }
    |> Seq.map (fun i -> m.[*, i])
    |> Seq.map mostFrequent
    |> Seq.toArray
    |> System.String

// Part2

let leastCommon xs =
    xs
    |> Seq.groupBy id
    |> Seq.map (fun (k, v) -> (k, Seq.length v))
    |> Seq.minBy snd
    |> fst

let part2 filepath =
    let m = matrix filepath
    let len = m |> Array2D.length2

    seq { 0..len-1 }
    |> Seq.map (fun i -> m.[*, i])
    |> Seq.map leastCommon
    |> Seq.toArray
    |> System.String

part2 "test.txt"