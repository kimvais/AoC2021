module AoC2021.Tests
open Xunit
open Xunit.Abstractions

open AoC2021.Day1
open AoC2021.Day2
open AoC2021.Day3
open AoC2021.Day4
open AoC2021.Day5

type Puzzles(o: ITestOutputHelper) =
    [<Fact>]
    let ``day 1`` () =
        Assert.Equal(7I, day1 "test1" ())
        Assert.Equal(5I, day1part2 "test1" ())
        Assert.Equal(1266I, day1 "1" ())
        Assert.Equal(1217I, day1part2 "1" ())

    [<Fact>]
    let ``day2 `` () =
        Assert.Equal(150I, day2 "test2" ())
        Assert.Equal(1804520I, day2 "2" ())
        Assert.Equal(900I, day2part2 "test2" ())
        Assert.Equal(1971095320I, day2part2 "2" ())
        
    [<Fact>]
    let ``day3 `` () =
        Assert.Equal(198I, day3 "test3" ())
        Assert.Equal(4160394I, day3 "3" ())
        Assert.Equal(230I, day3part2 "test3" ())
        
    [<Fact>]
    let ``day 4`` () =
        Assert.Equal(4512I, day4 "test4" ())
        Assert.Equal(54275I, day4 "4" ())
        Assert.Equal(1924I, day4part2 "test4" ())
        
    let ``day 5``() =
        Assert.Equal(5I, day5 "test5" ())
        Assert.Equal(6666I, day5 "5" ())
