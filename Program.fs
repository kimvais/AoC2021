module AoC2021.Main

open AoC2021.Utils
open AoC2021.Day1
open AoC2021.Day2
open AoC2021.Day3
open AoC2021.Day4
open AoC2021.Day5
open AoC2021.Day6
open AoC2021.Day7
open AoC2021.Day8

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
    | "5" -> day5 "5" ()
    | "6" -> day6 "6" 80 ()
    | "6b" -> day6 "6" 256 ()
    | "7" -> day7 "7"  ()
    | "7b" -> day7 "7"  ()
    | "8" -> day8 "8" ()
    | "test" -> day4part2 "4" ()
    |> printfn "%d"
    0
   