module AoC2021.Tests

open FsUnit.Xunit
open Xunit

open AoC2021.Utils
open AoC2021.Day1
open AoC2021.Day2
open AoC2021.Day3
open AoC2021.Day4
open AoC2021.Day5
open AoC2021.Day6
open AoC2021.Day7
open AoC2021.Day8
open AoC2021.Day9
open AoC2021.Day10
open AoC2021.Day11
open AoC2021.Day12
open AoC2021.Day13
open AoC2021.Day14
open AoC2021.Day15
open AoC2021.Day16
open AoC2021.Day17
open AoC2021.Day18
// open AoC2021.Day19
open AoC2021.Day20

[<Fact>]
let ``day 1, part 1`` () =
    day1 "test1" () |> should equal 7L
    day1part2 "test1" () |> should equal 5L

[<Fact>]
let ``day 1, part 2`` () =
    day1 "1" () |> should equal 1266L
    day1part2 "1" () |> should equal 1217L

[<Fact>]
let ``day 2, part 1`` () =
    day2 "test2" () |> should equal 150L
    day2 "2" () |> should equal 1804520L

[<Fact>]
let ``day 2, part 2`` () =
    day2part2 "test2" () |> should equal 900L
    day2part2 "2" () |> should equal 1971095320L

[<Fact>]
let ``day 3, part 1`` () =
    day3 "test3" () |> should equal 198L
    day3 "3" () |> should equal 4160394L

[<Fact>]
let ``day 3, part 2`` () =
    day3part2 "test3" () |> should equal 230L
    day3part2 "3" () |> should equal 4125600L

[<Fact>]
let ``day 4, part 1`` () =
    day4 "test4" () |> should equal 4512L
    day4 "4" () |> should equal 54275L

[<Fact>]
let ``day 4, part 2`` () =
    day4part2 "test4" () |> should equal 1924L
    day4part2 "4" () |> should equal 13158L

[<Fact>]
let ``day 5, part 1`` () =
    day5 "test5" () |> should equal 5L
    day5 "5" () |> should equal 6666L

[<Fact>]
let ``day 5, part 2`` () =
    day5part2 "test5" () |> should equal 12L
    day5part2 "5" () |> should equal 19081L


[<Fact>]
let ``day 6`` () =
    day6 "test6" 18 () |> should equal 26L
    day6 "test6" 80 () |> should equal 5934L
    day6 "6" 80 () |> should equal 343441L
    day6 "test6" 256 () |> should equal 26984457539L
    day6 "6" 256 () |> should equal 1569108373832L

[<Fact>]
let ``day 7, part 1`` () =
    day7 "test7" () |> should equal 37L
    day7 "7" () |> should equal 344138L

[<Fact>]
let ``day 7, part 2`` () =
    day7part2 "test7" () |> should equal 168L
    day7part2 "7" () |> should equal 94862124L

[<Fact>]
let ``day 8, part 1`` () =
    day8 "test8" () |> should equal 26L
    day8 "8" () |> should equal 274L

[<Fact>]
let ``day 8, part 2`` () =
    day8part2 "test8" () |> should equal 61229L
    day8part2 "8" () |> should equal 1012089L

[<Fact>]
let ``day 9, part 1`` () =
    day9 "test9" () |> should equal 15L
    day9 "9" () |> should equal 577L

[<Fact>]
let ``day 9, part 2`` () =
    day9part2 "test9" () |> should equal 1134L
    day9part2 "9" () |> should equal 1069200L

[<Fact>]
let ``day 10, part 1`` () =
    day10 "test10" () |> should equal 26397L
    day10 "10" () |> should equal 367227L

[<Fact>]
let ``day 10, part 2`` () =
    day10part2 "test10" () |> should equal 288957L
    day10part2 "10" () |> should equal 3583341858L

[<Fact>]
let ``day 11, part 1`` () =
    day11 "test11" 10 () |> should equal 204L
    day11 "test11" 100 () |> should equal 1656L
    day11 "11" 100 () |> should equal 1620L
    
[<Fact>]
let ``day 11, part 2`` () =
    day11part2 "test11" () |> should equal 195L
    day11part2 "11" () |> should equal 371L
    
[<Fact>]
let ``day 12, part 1`` () =
    day12 "test12a" () |> should equal 10L
    day12 "test12b" () |> should equal 19L
    day12 "test12c" () |> should equal 226L
    
[<Fact>]
let ``day 12, part 2`` () =
    day12part2 "test12a" () |> should equal 36L
    day12part2 "test12b" () |> should equal 103L
    day12part2 "test12c" () |> should equal 3509L
    
[<Fact>]
let ``day 13, part 1`` () =
    day13 "test13" () |> should equal 17L
    day13 "13" () |> should equal 695L

[<Fact>]
let ``day 14, part 1`` () =
    day14 "test14" 10 () |>should equal 1588L
    day14 "14" 10 () |>should equal 3143L
    day14 "test14" 40 () |> should equal 2188189693529L
    day14 "14" 40 () |> should equal 4110215602456L
    
[<Fact>]
let ``day 16, part 1`` () =
    ()
[<Fact>]
let ``day 16, part 2`` () =
    d16parse2 "C200B40A82" |> should equal 3L
    d16parse2 "04005AC33890" |> should equal 54L
    d16parse2 "880086C3E88112" |> should equal 7L
    d16parse2 "CE00C43D881120" |> should equal 9L
    d16parse2 "D8005AC2A8F0" |> should equal 1L
    d16parse2 "F600BC2D8F" |> should equal 0L
    d16parse2 "9C0141080250320F1802104A08" |> should equal 1L

[<Fact>]
let ``day 17 trajectories`` () =
    let launch, potentialVs = getParameters 20 30 -10 -5

    let vectors =
        potentialVs
        |> Seq.map (launch >> isHit2)
        |> Seq.choose id
    
    let input =
        readInput "test17b"
        |> Seq.map (splitS "\s+")
        |> Seq.concat
        |> Seq.map (split ',' >> fun [| a; b |] -> int a, int b)
        |> Set.ofSeq

    vectors
    |> Set.ofSeq
    |> Set.difference input |> should be Empty
    
[<Fact>]
let ``day 17, part 1`` () =
    day17 20 30 -10 -5 () |> should equal 45L
    day17 201 230 -99 -65 () |> should equal 4851L

[<Fact>]
let ``day 17, part 2`` () =
    day17part2 20 30 -10 -5 () |> should equal 112L
    day17part2  201 230 -99 -65 () |> should equal 1739L

[<Fact>]
let ``day 20, part 1`` () =
    day20 "test20" 2 () |> should equal 35L 
    day20 "20" 2 () |> should equal 5081L
    
[<Fact>]
let ``day 20, part 2`` () =
    day20 "test20" 50 () |> should equal 3351L
    day20 "20" 50 () |> should equal 15088L