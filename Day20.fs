module AoC2021.Day20

open System
open AoC2021.Utils

let symbolToBool =
    function
    | '.' -> false
    | '#' -> true
    | c -> failwith $"invalid character '%c{c}' in input"

let getArr (arr: bool [] []) def x y =
    match arr |> Array.tryItem x with
    | None -> def
    | Some r ->
        match r |> Array.tryItem y with
        | None -> def
        | Some p -> p

let coords x y =
    [ (x - 1, y - 1)
      (x - 1, y)
      (x - 1, y + 1)
      (x, y - 1)
      (x, y)
      (x, y + 1)
      (x + 1, y - 1)
      (x + 1, y)
      (x + 1, y + 1) ]

let padRow sym row =
    Array.concat [ [| sym; sym |]
                   row
                   [| sym; sym |] ]

let padImage sym (image: bool [] []) =
    let im = image |> Array.map (padRow sym)
    let rowLen = Array.length (Array.head im)
    let createEmptyRow () = Array.create rowLen sym

    Array.concat [| [| createEmptyRow () |]
                    [| createEmptyRow () |]
                    im
                    [| createEmptyRow () |]
                    [| createEmptyRow () |] |]

let enhancePixel (algo: bool array) def image x y _ =
    let i =
        coords x y
        |> List.collect (fun (x, y) -> [ (getArr image def x y) ])
        |> List.map (boolToSymbol 0 1)
        |> bitsToInt
        |> int

    algo.[i]

let printIm = printImage (boolToSymbol "." "#")

let enhanceImage algo sym image =
    printfn "Before padding: "
    printIm image
    let image' = image |> padImage sym

    printfn "After padding: "
    printIm image'

    let image'' =
        image'
        |> Array.mapi (fun x row -> (row |> Array.mapi (enhancePixel algo sym image' x)))

    printfn "After transform:"
    printIm image''
    image''

let generate rounds fn =
    let input = readInput fn
    let enchanceAlgorithm = input |> Seq.head |> Seq.map symbolToBool |> Array.ofSeq
    let flipper = function | false -> (fun _ -> false) | true-> (fun i -> i % 2 = 1)
    let enhance = enhanceImage enchanceAlgorithm
    let folder state i = enhance (flipper (Array.head enchanceAlgorithm) i) state

    let image =
        input
        |> Seq.skip 2
        |> Seq.map (Array.ofSeq >> Array.map symbolToBool)
        |> Array.ofSeq

    [ 0 .. (rounds - 1) ] |> Seq.fold folder image

let day20 fn rounds () =
    let image = generate rounds fn

    image
    |> Seq.concat
    |> Seq.sumBy
        (function
        | false -> 0
        | true -> 1)
    |> int64
