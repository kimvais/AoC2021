module AoC2021.Day7

open AoC2021.Utils

let day7 fn () =
    let input = readInput fn |> Seq.head

    let depths =
        input
        |> split ','
        |> Array.map (int >> float)
        |> Array.sort

    let idealDepth = depths.[Array.length depths / 2]

    depths
    |> Array.map (fun d -> int64 (abs (d - idealDepth)))
    |> Array.sumBy id

let fuelConsumption n = [ 1L .. n ] |> List.sum

let getFuelConsumption depths depth =
    let consumption = depths |> Array.map (fun d -> int64 (abs (d - depth))) |> Array.sumBy fuelConsumption
    // printfn "%d: %d" depth consumption
    depth, consumption

let day7part2 fn () =
    let input = readInput fn |> Seq.head

    let depths =
        input
        |> split ','
        |> Array.map int64
        |> Array.sort

    let minDepth = depths.[0]
    let maxDepth = depths.[Array.length depths - 1]

    [ minDepth .. maxDepth ]
    |> Seq.map (getFuelConsumption depths)
    |> Seq.minBy snd
    |> snd
