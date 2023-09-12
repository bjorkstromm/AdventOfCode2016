open System.Text.RegularExpressions

type Room = {
    Name: string
    SectorId: int
    Checksum: string
}

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let parseRoom (str : string) =
    match str with
    | ParseRegex @"^([a-z-]+)-([0-9]+)\[([a-z]+)\]$" [name; sectorId; checksum] ->
        Some { 
            Name = name
            SectorId = sectorId |> int
            Checksum = checksum
        }
    | _ -> None

let parseRooms path =
    System.IO.File.ReadAllLines(path)
    |> Array.choose parseRoom

let checksum room =
    room.Name
    |> Seq.filter (fun c -> c <> '-')
    |> Seq.countBy id
    |> Seq.sortBy (fun (c, count) -> -count, c)
    |> Seq.map fst
    |> Seq.take 5
    |> Seq.toArray
    |> System.String

let valid room =
    if room.Checksum = checksum room
    then Some room
    else None

let part1 path =
    path
    |> parseRooms
    |> Array.choose valid
    |> Array.sumBy (fun r -> r.SectorId)

// Part 2

let decrypt room =
    let rotate n c =
        if c = '-'
        then ' '
        else (((((c |> int) - 97) + n) % 26) + 97) |> char

    room.Name
    |> Seq.map (rotate room.SectorId)
    |> Seq.toArray
    |> System.String

let part2 path =
    path
    |> parseRooms
    |> Array.choose valid
    |> Array.map (fun r -> r |> decrypt, r.SectorId)
    |> Array.filter (fun (name, _) -> name.StartsWith "northpole")
    |> Array.map snd