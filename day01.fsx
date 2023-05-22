open System.IO

type Direction =
    | North
    | South
    | West
    | East

type Turn =
    | Left
    | Right

type Instruction = {
    Turn: Turn
    Distance: int
}

type Position = {
    X: int
    Y: int
    Direction: Direction
}

let parseInstruction (instruction: string) =
    let direction = match instruction.[0] with
                    | 'L' -> Turn.Left
                    | 'R' -> Turn.Right
                    | t -> failwithf "Invalid direction %c" t
    let distance = int instruction.[1..]
    { Turn = direction; Distance = distance }

let parseInstructions (instructions: string) =
    instructions.Split ([| ','; ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseInstruction

let changeDirection direction turn =
    match direction, turn with
    | North, Left -> West
    | North, Right -> East
    | South, Left -> East
    | South, Right -> West
    | West, Left -> South
    | West, Right -> North
    | East, Left -> North
    | East, Right -> South

let changePosition position direction distance =
    match direction with
    | North -> (position.X, position.Y + distance)
    | South -> (position.X, position.Y - distance)
    | West  -> (position.X - distance, position.Y)
    | East  -> (position.X + distance, position.Y)

let move position instruction =
    let newDirection = changeDirection position.Direction instruction.Turn
    let (newX, newY) = changePosition position newDirection instruction.Distance
    { X = newX; Y = newY; Direction = newDirection }

let distance position =
    abs position.X + abs position.Y

// Part 1
let part1 = 
    "day01.txt"
    |> System.IO.File.ReadAllText
    |> parseInstructions
    |> Array.fold move { X = 0; Y = 0; Direction = Direction.North }
    |> distance

// Part 2
let move2 instructions =
    let visited = System.Collections.Generic.HashSet<(int * int)>()
    visited.Add (0, 0) |> ignore

    let rec loop position instructions =
        match instructions with
        | [] -> position
        | instruction :: instructions ->
            let newDirection = changeDirection position.Direction instruction.Turn
            let newPositions =
                [1..instruction.Distance]
                |> List.map (fun i ->
                    let pos = changePosition position newDirection i
                    (pos, visited.Add pos |> not))

            match newPositions |> List.tryFind (fun (_, visited) -> visited) with
            | Some ((x, y), _) -> { X = x; Y = y; Direction = newDirection }
            | None ->
                let (x, y) = newPositions |> List.last |> fst
                let newPosition = { X = x; Y = y; Direction = newDirection }
                loop newPosition instructions

    instructions
    |> List.ofSeq
    |> loop { X = 0; Y = 0; Direction = Direction.North }

let part2 = 
    "day01.txt"
    |> System.IO.File.ReadAllText
    |> parseInstructions
    |> move2
    |> distance