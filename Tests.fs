module AoC2021.Tests
open Xunit
open Xunit.Abstractions

open AoC2021.Day1
open AoC2021.Day2

type Puzzles(o: ITestOutputHelper) =
    [<Fact>]
    let ``day 1`` () =
        Assert.Equal(7, day1 "test1" ())
        Assert.Equal(5, day1part2 "test1" ())
        Assert.Equal(1266, day1 "1" ())
        Assert.Equal(1217, day1part2 "1" ())

    [<Fact>]
    let ``day2 `` () =
        Assert.Equal(150, day2 "test2" ())
        Assert.Equal(900, day2part2 "test2" ())
