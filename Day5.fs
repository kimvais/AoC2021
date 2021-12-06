module AoC2021.Day5

open AoC2021.Utils

let vecToTuple vec =
    let startPoint = Seq.head vec
    let endPoint = Seq.last vec
    let x1 = Seq.head startPoint
    let x2 = Seq.head endPoint
    let y1 = Seq.last startPoint
    let y2 = Seq.last endPoint
    (x1, y1), (x2, y2)

let filterInvalidVectors vec =
    let (x1, y1), (x2, y2) = vecToTuple vec
    x1 = x2 || y1 = y2

let vectorToCoordinates ((x1, y1), (x2, y2)) =
    match x1 = x2 with
    | true ->
        let [ yA; yB ] = List.sort [ y1; y2 ]
        [ yA .. yB ] |> Seq.map (fun n -> (x1, n))
    | false ->
        let [ xA; xB ] = List.sort [ x1; x2 ]
        [ xA .. xB ] |> Seq.map (fun n -> (n, y1))

let plotVectors (vecMap: int [][]) coords =
    coords
    |> Seq.iter (fun (x, y) -> vecMap.[x].[y] <- (vecMap.[x].[y] + 1))

let day5 fn () =
    let input = readInput fn
    let lines = input |> Seq.map (splitS " -> ")

    let vectors =
        lines
        |> Seq.map (Seq.map ((split ',') >> Seq.map int))
        |> Seq.filter filterInvalidVectors
        |> Seq.cache

    let maxX =
        vectors
        |> Seq.concat
        |> Seq.map Seq.head
        |> Seq.max

    let maxY =
        vectors
        |> Seq.concat
        |> Seq.map Seq.last
        |> Seq.max

    let vectors' = vectors |> Seq.map vecToTuple
    // printfn "%A" vectors'
    // printfn "%A %A" maxX maxY
    let ventMap = Array.init (maxX + 1) (fun _ -> Array.create (maxY + 1) 0)
    // printfn "%A" ventMap
    let coords = vectors' |> Seq.map vectorToCoordinates
    coords |> Seq.iter (plotVectors ventMap)
    printfn "%A" ventMap
    ventMap |> Array.reduce Array.append |> Seq.filter ((<) 1) |> Seq.length |> int64
