module AoC2021.Main

open AoC2021.Utils
open AoC2021.Day1

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem
    match day with
    | "1" -> day1 "1" ()
    | "1b" -> day1part2 "1" ()
    |> printfn "%d"
    0
   