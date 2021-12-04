module AoC2021.Tests
open Xunit
open Xunit.Abstractions

open AoC2021.Day1
open AoC2021.Day2
open AoC2021.Day3
open AoC2021.Day4

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
        Assert.Equal(1804520, day2 "2" ())
        Assert.Equal(900, day2part2 "test2" ())
        Assert.Equal(1971095320, day2part2 "2" ())
        
    [<Fact>]
    let ``day3 `` () =
        Assert.Equal(198, day3 "test3" ())
        Assert.Equal(4160394, day3 "3" ())
        Assert.Equal(230, day3part2 "test3" ())
        
    [<Fact>]
    let ``day 4`` () =
        Assert.Equal(4512, day4 "test4" ())
        Assert.Equal(54275, day4 "4" ())
        Assert.Equal(1924, day4part2 "test4" ())