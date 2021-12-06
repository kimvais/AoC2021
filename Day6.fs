module AoC2021.Day6

open AoC2021.Utils

let generation age =
    match age with
    | 0 -> seq [6;8]
    | n -> seq [n-1]
    
let day6 fn () =
    let mutable fish = readInput fn |> Seq.head |> split ',' |> Seq.map int
    [0..79] |> Seq.iter (fun _ -> fish <- Seq.collect generation fish)
    printfn "%A" fish
    Seq.length fish 