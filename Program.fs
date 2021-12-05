module AoC2021.Main

open AoC2021.Utils
open AoC2021.Day1
open AoC2021.Day2
open AoC2021.Day3
open AoC2021.Day4
open AoC2021.Day5

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem
    match day with
    | "1" -> day1 "1" ()
    | "1b" -> day1part2 "1" ()
    | "2" -> day2 "2" ()
    | "2b" -> day2part2 "2" ()
    | "3" -> day3 "3" ()
    | "3b" -> day3part2 "3" ()
    | "4" -> day4 "4" ()
    | "4b" -> day4part2 "4" ()
    | "test" -> day5 "test5" ()
    |> printfn "%d"
    0
   