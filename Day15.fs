module AoC2021.Day15

open AoC2021.Utils

type Node =
    { X: int
      Y: int
      Risk: uint64
      Distance: uint64
      Previous: option<Node> }


let createNode x y risk =
    { X = x
      Y = y
      Risk = risk
      Distance = System.UInt64.MaxValue
      Previous = None }

let getAllNodes (grid: Node [] []) =
    grid
    |> Seq.concat
    |> Seq.map (fun n -> n.X, n.Y)
    |> Set.ofSeq

let getNode (grid: (Node) [] []) (x, y) () = grid.[x].[y]

let printRow row =
    row
    |> Array.map
        (fun n ->
            match n.Distance with
            | System.UInt64.MaxValue -> " .. "
            | n -> n |> sprintf "%3d ")
    |> String.concat ""
    |> printfn "%s"

let printGrid grid =
    grid |> Array.iter printRow
    printfn ""

let getNeighbourCoords x y =
    seq [ x - 1, y
          x, y - 1
          x + 1, y
          x, y + 1 ]

let rec dijkstra (grid: Node [] []) queue =
    match Set.isEmpty queue with
    | true -> grid
    | false ->
        let curr =
            queue
            |> Seq.minBy (fun (x, y) -> grid.[x].[y].Distance)
            |> getNode grid <| ()

        let queue' = Set.remove (curr.X, curr.Y) queue

        let neighbours =
            getNeighbourCoords curr.X curr.Y
            |> Seq.filter
                (fun (nx, ny) ->
                    nx >= 0
                    && ny >= 0
                    && nx < Array.length grid
                    && ny < Array.length (Array.head grid)
                    && (queue' |> Set.contains (nx, ny)))
            |> Seq.map (fun (x', y') -> getNode grid (x',y') ())

        neighbours
        |> Seq.iter
            (fun n ->
                let newDistance = curr.Distance + n.Risk

                if newDistance < n.Distance then

                    grid.[n.X].[n.Y] <-
                        { n with
                              Distance = newDistance
                              Previous = Some curr })

        dijkstra grid queue'

let day15 fn () =
    let parseRow x row =
        row
        |> Seq.mapi (fun y c -> c |> charToInt |> uint64 |> createNode x y)
        |> Array.ofSeq

    let grid =
        readInput fn
        |> Seq.mapi (fun x row -> row |> parseRow x)
        |> Array.ofSeq

    grid.[0].[0] <-
        { grid.[0].[0] with
              Risk = 0UL
              Distance = 0UL }

    (dijkstra grid (getAllNodes grid) |> Array.last |> Array.last).Distance |> int64
