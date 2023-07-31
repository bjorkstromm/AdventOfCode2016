let triangle (a, b, c) =
    let xs = [|a; b; c|] |> Array.sort
    if xs.[0] + xs.[1] > xs.[2]
    then Some (a, b, c)
    else None

let parse (line: string) =
    let xs =
        line.Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
    match xs with
    | [|a; b; c|] -> (a, b, c)
    | _ -> failwithf "Invalid input: %s" line

let parseFile (path: string) =
    System.IO.File.ReadAllLines path
    |> Array.map parse

// Part 1
let part1 (path: string) =
    parseFile path
    |> Array.choose triangle
    |> Array.length

// Part 2

let parse2 (lines: string[]) =
    let parseLine (line : string) = 
        line.Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    lines
        |> Array.chunkBySize 3
        |> Array.map (fun xs ->
            xs |> Array.map parseLine
               |> Array.transpose)
        |> Array.concat
        |> Array.map (fun xs ->
            match xs with
            | [|a; b; c|] -> (a, b, c)
            | _ -> failwithf "Invalid input: %A" xs)

let parseFile2 (path: string) =
    System.IO.File.ReadAllLines path
    |> parse2

let part2 (path: string) =
    parseFile2 path
    |> Array.choose triangle
    |> Array.length