module AoC2021.Tests
open Xunit
open Xunit.Abstractions

open AoC2021.Day1

type Puzzles(o: ITestOutputHelper) =
    [<Fact>]
    let ``day 1`` () =
        Assert.Equal(7, day1 "test1" ())
        Assert.Equal(5, day1part2 "test1" ())
        Assert.Equal(1266, day1 "1" ())
        Assert.Equal(1217, day1part2 "1" ())
