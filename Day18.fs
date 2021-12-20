module AoC2021.Day18

open AoC2021.Utils
open Chiron


let rec parse acc depth (data: list<char>) =
    match data with
    | [] -> acc
    | head :: tail ->
        match head with
        | ']' -> parse acc (depth - 1) tail
        | '[' -> parse acc (depth + 1) tail
        | ',' -> parse acc depth tail
        | n -> parse (acc @ [ (charToL n, depth) ]) depth tail

let day18 s () =
    parse [] 0 (List.ofSeq s) |> List.groupBy snd |> printfn "%A"
    0L