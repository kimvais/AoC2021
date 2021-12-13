module AoC2021.Day9

open AoC2021.Utils

let getNeighbours map x y =
    seq [ (x + 1, y)
          (x - 1, y)
          (x, y - 1)
          (x, y + 1) ]
    |> Seq.map
        (function
        | x', y' ->
            let row = Array.tryItem x' map

            match row with
            | None -> None
            | Some r ->
                match Array.tryItem y' r with
                | None -> None
                | Some _ -> Some(x', y'))
    |> Seq.choose id

let getAllPits map =
    let lenX = Array.length map - 1
    let lenY = Array.length (Array.head map) - 1

    seq [ [ 0 .. lenX ]
          |> Seq.map
              (fun x ->
                  [ 0 .. lenY ]
                  |> Seq.map
                      (fun y ->
                          let depth = map.[x].[y]
                          let neighbours = getNeighbours map x y

                          match neighbours
                                |> Seq.forall (fun (x, y) -> map.[x].[y] > depth) with
                          | true -> Some(x, y)
                          | false -> None)) ]
    |> Seq.concat
    |> Seq.concat
    |> Seq.choose id


let getMap fn =
    readInput fn
    |> Array.ofSeq
    |> Array.map (Seq.map charToL >> Array.ofSeq)

let day9 fn () =
    let heightMap = getMap fn

    getAllPits heightMap
    |> Seq.map (fun (x, y) -> heightMap.[x].[y] + 1L)
    |> Seq.sum

let rec getBasin map basin (x, y) =

    let basin' = Set.add (x, y) basin

    let neighbours =
        getNeighbours map x y
        |> Seq.filter
            (fun (x', y') ->
                map.[x'].[y'] < 9L
                && map.[x].[y] < map.[x'].[y']
                && not (Set.contains (x', y') basin'))
        |> Set.ofSeq

    let basin'' = Set.union basin' neighbours

    match Set.isEmpty neighbours with
    | true -> basin'
    | false ->
        // printfn "(%d, %d): %d" x y (Set.count basin'')
        neighbours
        |> Seq.map (getBasin map basin'')
        |> Set.unionMany


let day9part2 fn () =
    let heightMap = getMap fn
    let pits = getAllPits heightMap

    let basins =
        pits
        |> Seq.map (getBasin heightMap Set.empty<int * int>)
        |> Seq.map Set.count
        |> Seq.sortBy (~-)

    printfn "%A" basins

    basins
    |> Seq.take 3
    |> Seq.map int64
    |> Seq.reduce (*)
