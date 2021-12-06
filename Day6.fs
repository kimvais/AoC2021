module AoC2021.Day6

open AoC2021.Utils

let generation (age:int, count:int64) =
    match age with
    | 0 -> seq [ (6, count); (8, count) ]
    | n -> seq [ (n - 1, count) ]

let merge (fishCounts: seq<int * int64>) =
    seq [ for age in [ 0 .. 8 ] do
              let n =
                  fishCounts
                  |> Seq.filter (fst >> (=) age)
                  |> Seq.sumBy snd

              yield (age, n) ]

let day6 fn n () =
    let mutable fish =
        readInput fn
        |> Seq.head
        |> split ','
        |> Seq.map int
        |> Seq.countBy id
        |> Seq.map (fun (a,c) -> (a, int64 c))

    let runGeneration _ =
        // printfn "%A" fish
        fish <- Seq.collect generation fish |> merge

    [ 0 .. (n - 1) ]
    |> Seq.iter runGeneration

    Seq.sumBy snd fish
