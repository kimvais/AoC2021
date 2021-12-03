module AoC2021.Day3

open System
open AoC2021.Utils

let convertCountsToOnesAndZeroes len n =
    float n > len / 2.0
    |> function
        | true -> "1"
        | false -> "0"

let asNumber s = Convert.ToInt32(s, 2)

let day3 fn () =
    let input = readInput fn
    let len = Seq.length input
    let bits = input |> Seq.transpose
    let mask = String.init (Seq.length bits) (fun _ -> "1") |> asNumber

    let bitCounts =
        bits
        |> Seq.map (Seq.fold (fun a i -> a + (int i - int '0')) 0)

    let converter =
        convertCountsToOnesAndZeroes (len |> float)

    let gamma = bitCounts |> Seq.map converter |> String.concat "" |> asNumber
 
    let epsilon = gamma ^^^ mask
         
    gamma * epsilon 

