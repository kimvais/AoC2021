module AoC2021.Day15

open AoC2021.Utils

type Node =
    { X: int
      Y: int
      Risk: uint64
      Distance: uint64
      Visited: bool }


let createNode x y risk =
    { X = x
      Y = y
      Risk = risk
      Distance = System.UInt64.MaxValue
      Visited = false }

let getAllNodes (grid: Node [] []) = grid |> Seq.concat

let getNode (grid: (Node) [] []) (x, y) = grid.[x].[y]

let calcDistanceAndUpdate grid distance (x, y) =
    let cn = getNode grid (x, y)

    let distance' =
        List.min [ distance + cn.Risk
                   cn.Distance ]
    let cn' =
        { cn with
              Distance = distance' }

    grid.[cn'.X].[cn'.Y] <- cn'
    cn'

let markNext grid n =
    let maxX = Array.length grid
    let maxY = Array.length (Array.head grid)

    seq [ n.X + 1, n.Y; n.X, n.Y + 1 ]
    |> Seq.filter (fun (x', y') -> x' < maxX && y' < maxY)
    |> Seq.map (calcDistanceAndUpdate grid n.Distance)

let getNextNode grid =
    let nodes = getAllNodes grid

    nodes
    |> Seq.filter (fun n -> n.Visited)
    |> Seq.map (markNext grid)
    |> Seq.concat
    |> Seq.filter (fun n -> not n.Visited)
    |> Seq.minBy (fun n -> n.Distance)

let printRow row =
    row
    |> Array.map
        (fun n ->
            match n.Distance with
            | System.UInt64.MaxValue -> " ... "
            | n -> n |> sprintf "%4d ")
    |> String.concat ""
    |> printfn "%s"

let printGrid grid =
    grid |> Array.iter printRow
    printfn ""

let getUnvisited (grid: Node [] []) =
    grid
    |> getAllNodes
    |> Seq.filter (fun n -> not n.Visited)

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
              Distance = 0UL
              Visited = true }

    let rec traverse (grid: Node [] []) =
        match Seq.isEmpty (getUnvisited grid) with
        | false ->
            let node = getNextNode grid
            let distance = node.Distance
            printfn "%d,%d" node.X node.Y

            let distance' =
                List.min [ distance + node.Risk
                           node.Distance ]

            grid.[node.X].[node.Y] <-
                { node with
                      Distance = distance'
                      Visited = true }

            printGrid grid

            traverse grid
        | true -> grid


    // unvisitedNodes |> printfn "%A"
    traverse grid
    0L
