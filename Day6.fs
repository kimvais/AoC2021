module AoC2021.Day6

open AoC2021.Utils

let generation age =
    match age with
    | 0 -> seq [ 6; 8 ]
    | n -> seq [ n - 1 ]

let day6 fn n () =
    let mutable fish =
        readInput fn
        |> Seq.head
        |> split ','
        |> Seq.map int

    let runGeneration _ =
        printfn "%A" (Seq.length fish)
        fish <- Seq.collect generation fish
    [ 0 .. (n - 1) ] |> Seq.iter runGeneration
    Seq.length fish
