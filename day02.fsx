type Direction =
    | Up
    | Down
    | Left
    | Right

let parseInstructions (line: string) =
    line.ToCharArray()
    |> Seq.map (function
                   | 'U' -> Up
                   | 'D' -> Down
                   | 'L' -> Left
                   | 'R' -> Right
                   | d -> failwithf "Invalid direction %c" d)
    |> Seq.toList

let parseFile (path: string) =
    System.IO.File.ReadAllLines path
    |> Array.map parseInstructions

let keyPad =
    [|
        [| '1'; '2'; '3' |]
        [| '4'; '5'; '6' |]
        [| '7'; '8'; '9' |]
    |]
    |> array2D

let isValid (x, y) =
    x >= 0 && x < (keyPad |> Array2D.length2) && y >= 0 && y < (keyPad |> Array2D.length1)

let move (x, y) direction =
    match direction with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let rec getButton isValid (x, y) directions =
    match directions with
    | [] -> (x, y)
    | direction :: directions ->
        let (x', y') = move (x, y) direction
        if isValid (x', y') then getButton isValid (x', y') directions
        else getButton isValid (x, y) directions

let exec isValid (keyPad : char[,]) =
    "day02.txt"
    |> parseFile
    |> Array.fold (fun btns directions ->
                              let (x, y) = btns |> List.head
                              let (x', y') = getButton isValid (x, y) directions
                              (x', y') :: btns) [(1, 1)]
    |> List.rev
    |> List.skip 1
    |> List.map (fun (x, y) -> keyPad.[y, x])
    |> Array.ofList
    |> System.String

let part1 = exec isValid keyPad

// Part 2

let keyPad2 =
    [|
        [| ' '; ' '; '1'; ' '; ' ' |]
        [| ' '; '2'; '3'; '4'; ' ' |]
        [| '5'; '6'; '7'; '8'; '9' |]
        [| ' '; 'A'; 'B'; 'C'; ' ' |]
        [| ' '; ' '; 'D'; ' '; ' ' |]
    |]
    |> array2D

let isValid2 (x, y) =
    x >= 0
    && x < (keyPad2 |> Array2D.length2)
    && y >= 0
    && y < (keyPad2 |> Array2D.length1)
    && keyPad2.[y, x] <> ' '

let part2 = exec isValid2 keyPad2