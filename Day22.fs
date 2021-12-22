module AoC2021.Day22

open System.Runtime.InteropServices
open AoC2021.Utils

let clipWithin min max coordPair =
    coordPair
    |> (fun (c1, c2) -> List.max [ c1; min ], List.min [ c2; max ])

type Instruction =
    | On
    | Off

let parseCoordinate cs =
    cs
    |> splitS "\.\."
    |> Seq.map int
    |> List.ofSeq
    |> fun [ c1; c2 ] -> (c1, c2)

let parseInstruction row =
    let [ inst; coordinateData ] = (row |> split ' ' |> List.ofSeq)

    let inst' =
        inst
        |> (function
        | "on" -> On
        | "off" -> Off)

    let coords =
        split ',' coordinateData
        |> Seq.map (split '=' >> Seq.last >> parseCoordinate >> clipWithin -50 50)

    inst', coords

let coordsToRange = Seq.map (fun (c1, c2) -> [ c1 .. c2 ])
let toTuple [ x; y; z ] = x, y, z

let getAllCubes =
    coordsToRange
    >> List.ofSeq
    >> cartesian
    >> Seq.map toTuple
    >> Set.ofSeq

let turnOnOrOff state (instr, coords) =
    match instr with
    | On -> Set.union state (getAllCubes coords)
    | Off -> Set.difference state (getAllCubes coords)

let day22 fn () =
    let input = readInput fn
    let instructions = input |> Seq.map parseInstruction

    instructions
    |> Seq.fold turnOnOrOff Set.empty<int * int * int>
    |> Set.count
    |> int64
