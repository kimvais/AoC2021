module AoC2021.Main

open AoC2021.Utils
open AoC2021.Day1
open AoC2021.Day2
open AoC2021.Day3

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem
    match day with
    | "1" -> day1 "1" ()
    | "1b" -> day1part2 "1" ()
    | "2" -> day2 "2" ()
    | "2b" -> day2part2 "2" ()
    | "3" -> day3 "3" ()
    |> printfn "%d"
    0
   