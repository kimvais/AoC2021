module AoC2021.Day3

open System
open AoC2021.Utils

let convertCountsToOnesAndZeroes len n =
    float n > len / 2.0
    |> function
        | true -> "1"
        | false -> "0"

let asNumber s = Convert.ToInt32(s, 2)


let getMask len =
    String.init len (fun _ -> "1") |> asNumber

let getBits input = input |> Seq.transpose

let getEpsilon len gamma =
    let mask = getMask len
    gamma ^^^ mask


let getGamma len bits =
    let bitCounts =
        bits
        |> Seq.map (Seq.fold (fun a i -> a + (int i - int '0')) 0)

    let converter =
        convertCountsToOnesAndZeroes (len |> float)

    bitCounts
    |> Seq.map converter
    |> String.concat ""
    |> asNumber

let day3 fn () =
    let input = readInput fn
    let bits = getBits input
    let len = Seq.length input
    let gamma = getGamma len bits
    let epsilon = getEpsilon (Seq.length bits) gamma

    gamma * epsilon |> int64

let getDigit preferred (m: Map<char, int>) =
    let ones = m.['1']
    let zeroes = m.['0']

    match preferred with
    | 1 ->
        match ones - zeroes with
        | n when n >= 0 -> '1'
        | _ -> '0'
    | 0 ->
        match ones - zeroes with
        | n when n < 0 -> '1'
        | _ -> '0'


let rec countAndMatchBits preferred (input: seq<string>, index) =
    match Seq.tryExactlyOne input with
    | Some s -> s
    | None ->
        let mostCommon =
            input
            |> Seq.map (fun s -> s.[index])
            |> Seq.countBy id
            |> Map.ofSeq
            |> getDigit preferred

        let matching =
            input
            |> Seq.filter (fun s -> s.[index] = mostCommon)

        countAndMatchBits preferred (matching, index + 1)

let day3part2 fn () =
    let input = readInput fn

    let o2 =
        countAndMatchBits 1 (input, 0) |> asNumber

    let co2 =
        countAndMatchBits 0 (input, 0) |> asNumber

    o2 * co2 |> int64
