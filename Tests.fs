module AoC2021.Tests

open FsUnit.Xunit
open Xunit

open AoC2021.Day1
open AoC2021.Day2
open AoC2021.Day3
open AoC2021.Day4
open AoC2021.Day5
open AoC2021.Day6
open AoC2021.Day7
open AoC2021.Day8

[<Fact>]
let ``day 1`` () =
    day1 "test1" () |> should equal 7L
    day1part2 "test1" () |> should equal 5L
    day1 "1" () |> should equal 1266L
    day1part2 "1" () |> should equal 1217L

[<Fact>]
let ``day2 `` () =
    day2 "test2" () |> should equal 150L
    day2 "2" () |> should equal 1804520L
    day2part2 "test2" () |> should equal 900L
    day2part2 "2" () |> should equal 1971095320L

[<Fact>]
let ``day3 `` () =
    day3 "test3" () |> should equal 198L
    day3 "3" () |> should equal 4160394L
    day3part2 "test3" () |> should equal 230L

[<Fact>]
let ``day 4`` () =
    day4 "test4" () |> should equal 4512L
    day4 "4" () |> should equal 54275L
    day4part2 "test4" () |> should equal 1924L

[<Fact>]
let ``day 5`` () =
    day5 "test5" () |> should equal 5L
    day5 "5" () |> should equal 6666L
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
let ``day 7`` () =
    day7 "test7" () |> should equal 37L
    day7 "7" () |> should equal 344138L
    day7part2 "test7" () |> should equal 168L
    
[<Fact>]
let ``day8 ``() =
    day8 "test8" () |> should equal 26L
    day8 "8" () |> should equal 274L
    day8part2 "test8" () |> should equal 61229L
    day8part2 "8" () |> should equal 1012089L
