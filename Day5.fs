module AoC2021.Day5

open AoC2021.Utils

let day5 fn () =
    let input = readInput fn
    let lines = input |>  Seq.map (splitS " -> ")
    let vectors = lines |> Seq.map (Seq.map ((split ',') >> Seq.map int)) |> Seq.cache
    let maxX = vectors |> Seq.concat  |> Seq.map Seq.head |> Seq.max
    let maxY = vectors |> Seq.concat |> Seq.map Seq.last |> Seq.max
    printfn "%A" vectors
    printfn "%A %A" maxX maxY
    0