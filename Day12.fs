module AoC2021.Day12

open System
open AoC2021.Utils

let isUpper = String.forall Char.IsUpper

let isValid visited cave =
    isUpper cave
    || not (Seq.exists ((=) cave) visited)

let isValid2 visited cave =
    cave <> "start"
    && (isUpper cave
        || not (Seq.exists ((=) cave) visited)
        || visited
           |> Seq.countBy id
           |> Seq.exists (fun (cave, count) -> not (isUpper cave) && count = 2)
           |> not)

let rec traverse checkValidity (edges: Map<string, Set<string>>) (visited: string list) =
    let currentCave = List.last visited

    let exits =
        edges.[currentCave]
        |> Seq.filter (checkValidity visited)
        |> List.ofSeq

    seq [ for exit in exits do
              if exit = "end" then
                  yield visited @ [ "end" ]
              else
                  yield! traverse checkValidity edges (visited @ [ exit ]) ]

let getEdges fn =
    let input = readInput fn

    let tunnels =
        input
        |> Seq.map (
            split '-'
            >> List.ofArray
            >> fun [ starts; ends ] -> (starts, ends)
        )
        |> Seq.cache

    let forwards =
        tunnels
        |> Seq.groupBy fst
        |> Seq.map (fun (s, es) -> (s, es |> Seq.map snd |> Set.ofSeq))

    let backwards =
        tunnels
        |> Seq.groupBy snd
        |> Seq.map
            (fun (s, es) ->
                (s,
                 es
                 |> Seq.map fst
                 |> Seq.filter ((<>) "start")
                 |> Set.ofSeq))

    Seq.append forwards backwards
    |> Seq.groupBy fst
    |> Seq.map (fun (s, es) -> (s, es |> Seq.map snd |> Set.unionMany))
    |> Map.ofSeq

let day12 fn () =
    let edges = getEdges fn

    traverse isValid edges [ "start" ]
    |> Seq.length
    |> int64

let day12part2 fn () =
    let edges = getEdges fn

    traverse isValid2 edges [ "start" ]
    |> Seq.length
    |> int64
