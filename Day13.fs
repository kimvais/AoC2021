module AoC2021.Day13

open System
open AoC2021.Utils



let parse fn =
    let [ dotInput; instructionInput ] =
        readInput fn
        |> String.concat "\n"
        |> splitByTwoLinefeeds
        |> List.ofArray
        |> List.map splitByLinefeed

    let dots =
        dotInput
        |> Array.map (
            split ','
            >> Array.map int
            >> (fun [| a; b |] -> (a, b))
        )

    let folds =
        instructionInput
        |> Array.map (
            split ' '
            >> Array.last
            >> split '='
            >> fun [| a; b |] -> (a, int b)
        )

    let maxCoords =
        folds
        |> Array.groupBy fst
        |> Seq.map (fun (d, coords) -> d, coords |> Seq.map snd |> Seq.max)
        |> Map.ofSeq

    let sizeX = maxCoords.["x"] * 2 + 1
    let sizeY = maxCoords.["y"] * 2 + 1
    // printfn "%d %d" sizeX sizeY
    let paper =
        Array.init sizeY (fun _ -> (Array.create sizeX false))

    dots
    |> Array.iter (fun (x, y) -> paper.[y].[x] <- true)

    paper, folds

let joinHalves first second =
    let paper =
        Array.zip first second
        |> Array.map
            (fun (a, b) ->
                Array.zip a b
                |> Array.map (fun (c, d) -> (c || d)))

    // printPaper paper
    paper

let foldY y paper =
    let top = paper |> Array.take y
    let bottom = paper |> Array.skip (y + 1) |> Array.rev

    joinHalves top bottom

let foldX x paper =
    let left = paper |> Array.map (Array.take x)

    let right =
        paper
        |> Array.map (Array.skip (x + 1) >> Array.rev)

    joinHalves left right

let pickFold (dir, line) =
    match dir with
    | "x" -> foldX line
    | "y" -> foldY line

let folder paper instruction = pickFold instruction paper

let day13 fn () =
    let paper, folds = parse fn

    folds
    |> Array.take 1
    |> Array.fold folder paper
    |> Array.concat
    |> Array.sumBy Convert.ToInt64

let day13part2 fn () =
    let paper, folds = parse fn
    folds |> Array.fold folder paper |> printImage (boolToSymbol "0" "\u2588")
    0L
