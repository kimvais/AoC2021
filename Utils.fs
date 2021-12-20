module AoC2021.Utils

open System
open System.IO
open System.Text.RegularExpressions

let readLines filePath = File.ReadLines(filePath)

let readInput (s: string) =
    readLines (__SOURCE_DIRECTORY__ + (sprintf "/input/%s.txt" s))

let getProblem (a: seq<string>) : string = a |> Seq.head

module Seq =
    let repeatForever s =
        let c = Seq.cache s

        seq {
            while true do
                yield! c
        }

    let filteri f =
        Seq.mapi (fun i v -> (i, v))
        >> Seq.filter (fun v -> f (fst v) (snd v))
        >> Seq.map snd

let split (c: char) (s: string) = s.Split c
let splitS (sep: string) (s: string) = Regex.Split(s, sep)

let splitByLinefeed (s: string) = s.Split '\n'

let splitByTwoLinefeeds s = Regex.Split(s, "\n\n")

let readInputDelimByEmptyLine inputfile =
    readInput inputfile
    |> String.concat "\n"
    |> splitByTwoLinefeeds

let charToL (c: char) = int64 c - int64 '0'

let charToInt = charToL >> int

let hexToBits (value: seq<char>) =
    value
    |> Seq.map
        (fun n ->
            $"%04d{Convert.ToString(Convert.ToInt16(n.ToString(), 16), 2)
                   |> int}")
    |> String.concat ""
    |> Seq.map (fun c -> int c - int '0')

let hexToBits2 value =
    let raw =
        Convert.ToString(Convert.ToInt64(value.ToString(), 16), 2)
        |> Seq.map charToInt

    match Seq.length raw % 8 with
    | 6 -> Seq.append [ 0; 0 ] raw
    | 0 -> raw
    | _ -> failwith "Invalid input"

let bitsToInt bits =
    let s =
        bits |> Seq.map string |> String.concat ""

    Convert.ToInt64(s, 2)

let boolToSymbol falseC trueC =
    function
    | false -> falseC
    | true -> trueC

let printImage boolToString (image: bool [] []) =
    image
    |> Array.iter
        (fun row ->
            (row
             |> Array.map boolToString
             |> String.concat ""
             |> printfn "%s"))

    printfn ""
