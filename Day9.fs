module AoC2021.Day9

open AoC2021.Utils

let getNeighbours map x y =
    seq  [(x+1, y); (x-1, y); (x, y-1); (x, y+1)] |> Seq.map (
        function x', y' ->
           let row = Array.tryItem x' map
           match row with
           | None -> None
           | Some r ->
               match Array.tryItem y' r with
               | None -> None
               | Some d -> Some d
        ) |> Seq.choose id

let getAllPits map =
    let lenX = Array.length map - 1
    let lenY = Array.length (Array.head map) - 1
    seq [
        [0..lenX] |> Seq.map (fun x ->
            [0..lenY] |> Seq.map (fun y ->
                let depth = map.[x].[y]
                let neighbours = getNeighbours map x y
                match neighbours |> Seq.forall (fun h -> h > depth) with
                |true -> Some depth
                |false -> None
                )
            )
        ] |> Seq.concat |> Seq.concat |> Seq.choose id
    
let day9 fn () =
    let input = readInput fn
    let heightMap = input |> Array.ofSeq |> Array.map (Seq.map charToL >> Array.ofSeq)
    getAllPits heightMap |> Seq.map ((+) 1L) |> Seq.sum
    