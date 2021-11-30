module AoC2021.Tests
open Xunit
open Xunit.Abstractions

open AoC2021.Day1

type Puzzles(o: ITestOutputHelper) =
    [<Fact>]
    let ``day 1`` () =
        Assert.Equal(0, day1 ())
        Assert.Equal(0, day1part2 ())