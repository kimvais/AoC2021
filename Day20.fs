module AoC2021.Day20

open System
open AoC2021.Utils

let symbolToBool =
    function
    | '.' -> false
    | '#' -> true
    | c -> failwith $"invalid character '%c{c}' in input"

let checkMargins image =
    let top = image |> Array.head |> Seq.exists id
    let bottom = image |> Array.last |> Seq.exists id
    let left = image |> Array.map Array.head |> Seq.exists id
    let right = image |> Array.map Array.last |> Seq.exists id
    top, bottom, left, right

let getArr (arr: bool [] []) x y =
    match arr |> Array.tryItem x with
    | None -> false
    | Some r ->
        match r |> Array.tryItem y with
        | None -> false
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

let padRow sym left right row =
    match left, right with
    | true, true ->
        Array.concat [ [| sym |]
                       row
                       [| sym |] ]
    | true, false -> Array.concat [ [| sym |]; row ]
    | false, true -> Array.concat [ row; [| sym |] ]
    | _ -> row

let padImage sym (image: bool [] []) =
    let top, bottom, left, right = checkMargins image
    let im = image |> Array.map (padRow sym left right)
    let rowLen = Array.length (Array.head im)
    let createEmptyRow () = Array.create rowLen sym 

    match top, bottom with
    | true, true ->
        Array.concat [| [| createEmptyRow () |]
                        im
                        [| createEmptyRow () |] |]
    | true, false ->
        Array.concat [| [| createEmptyRow () |]
                        im |]
    | false, true ->
        Array.concat [| im
                        [| createEmptyRow () |] |]
    | false, false -> im

let enhancePixel (algo: bool array) image x y _ =
    let i =
        coords x y
        |> List.collect (fun (x, y) -> [ (getArr image x y) ])
        |> List.map (boolToSymbol 0 1)
        |> bitsToInt
        |> int

    algo.[i]

let printIm = printImage (boolToSymbol "." "#")

let enhanceImage algo sym image =
    let image' = image |> padImage sym

    printIm image'
    let image'' =
        image'
        |> Array.mapi (fun x row -> (row |> Array.mapi (enhancePixel algo image' x)))
    printIm image''
    image''

let generate rounds fn =
    let input = readInput fn
    let enchanceAlgorithm = input |> Seq.head |> Seq.map symbolToBool |> Array.ofSeq
    let enhance  = enhanceImage enchanceAlgorithm
    let folder state i = enhance ((i%2 = 1) <> (Array.head enchanceAlgorithm)) state

    let image =
        input
        |> Seq.skip 2
        |> Seq.map (Array.ofSeq >> Array.map symbolToBool)
        |> Array.ofSeq

    [ 0 .. (rounds-1) ] |> Seq.fold folder image

let solve rounds fn =
    let image = generate rounds fn

    image
    |> Seq.concat
    |> Seq.sumBy
        (function
        | false -> 0
        | true -> 1)
    |> int64

let day20 fn () = solve 2 fn
