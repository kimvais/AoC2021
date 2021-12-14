module AoC2021.Day14

open AoC2021.Utils
open System

let insertOne rules' ((a, b), c) =
    match rules' |> Map.tryFind (a, b) with
    | Some [ pair1; pair2 ] -> seq [ (pair1, c); (pair2, c) ]
    | None -> failwith "invalid rule"

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
        |> Array.map (
            splitS " -> "
            >> Array.map Array.ofSeq
            >> (fun [| [| a; b |]; [| c |] |] -> (a, b), [ (a, c); (c, b) ])
        )

    template', rules' |> Map.ofArray

let mergeCounts (letter, counts) = letter, counts |> Seq.sumBy snd

let insert rules counts =
    counts
    |> Map.toSeq
    |> Seq.map (insertOne rules)
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map mergeCounts
    |> Map.ofSeq

let rec polymerize rules rounds counts =
    match rounds with
    | 0 -> counts
    | n -> insert rules counts |> polymerize rules (n - 1)

let day14 fn rounds () =
    let template, rules = parseInput fn

    let counts =
        template
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> (a, b), 1L)
        |> Seq.groupBy fst
        |> Seq.map mergeCounts

    printfn "%A" counts

    let frequencies =
        polymerize rules rounds (counts |> Map.ofSeq)
        |> Map.toList

    let firsts =
        frequencies
        |> Seq.map (fun (a, c) -> fst a, c)
        |> Seq.groupBy fst
        |> Seq.map mergeCounts

    let seconds =
        frequencies
        |> Seq.map (fun (a, c) -> snd a, c)
        |> Seq.groupBy fst
        |> Seq.map mergeCounts



    let letterCounts =
        Seq.zip firsts seconds
        |> Seq.map (fun ((l, f), (_, s)) -> l, Seq.max [ f; s ])

    let mostCommon = letterCounts |> Seq.map snd |> Seq.max
    let leastCommon = letterCounts |> Seq.map snd |> Seq.min
    mostCommon - leastCommon
