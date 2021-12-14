module AoC2021.Day14

open AoC2021.Utils
open System

let addToEnd i s =
    Seq.append s (Seq.singleton i)
    
let insertOne rules' (a, b) =
    let key = $"%c{a}%c{b}"

    match rules' |> Map.tryFind key with
    | Some c -> $"%c{a}%s{c}"
    | None -> $"%c{a}"

let parseInput fn =
    let [ template; rules ] =
        readInput fn
        |> String.concat "\n"
        |> splitByTwoLinefeeds
        |> List.ofArray
        |> List.map splitByLinefeed

    let template' = template |> Array.head

    let rules' =
        rules
        |> Array.map ((splitS " -> ") >> fun [| a; b |] -> (a, b))
        |> Map.ofArray
    template', rules'
    
let insert rules t =
    t
    |> Seq.pairwise
    |> Seq.map (insertOne rules)
    |> Seq.concat
    |> addToEnd (Seq.last t)
    |> String.Concat

let rec polymerize rules rounds template =
    match rounds with
    | 0 -> template
    | n -> insert rules template |> polymerize rules (n-1)
    
let day14 fn rounds () =
    let template, rules = parseInput fn 

    let counts = polymerize rules rounds template |> Seq.countBy id |> Seq.map snd
    Seq.max counts - Seq.min counts |> int64
