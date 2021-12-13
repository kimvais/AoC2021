module AoC2021.Day1

open AoC2021.Utils

let increases tup = fst tup < snd tup

let day1 fn () =
    let input = readInput fn |> Seq.map int

    input
    |> Seq.pairwise
    |> Seq.filter increases
    |> Seq.length
    |> int64


let day1part2 fn () =
    let input = readInput fn |> Seq.map int

    input
    |> Seq.windowed 3
    |> Seq.map Array.sum
    |> Seq.pairwise
    |> Seq.filter increases
    |> Seq.length
    |> int64
