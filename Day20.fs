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

let getArr arr x y =
    let row =
        match arr |> Array.tryItem x with
        | None -> Array.create (arr |> Array.head |> Array.length) false
        | Some r -> r

    match row |> Array.tryItem y with
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

let padRow left right row =
    match left, right with
    | true, true ->
        Array.concat [ [| false |]
                       row
                       [| false |] ]
    | true, false -> Array.concat [ [| false |]; row ]
    | false, true -> Array.concat [ row; [| false |] ]
    | _ -> row

let padImage top bottom (im: bool [] []) =
    let rowLen = Array.length (Array.head im)
    let createEmptyRow () = (Array.create rowLen false)

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
    | _ -> im

let enhancePixel (algo: bool array) image x y _ =
    let i =
        coords x y
        |> List.collect (fun (x, y) -> [ (getArr image x y) ])
        |> List.map (boolToSymbol 0 1)
        |> bitsToInt
        |> int

    algo.[i]

let printIm = printImage (boolToSymbol "." "#")

let enhanceImage algo image =
    let top, bottom, left, right = checkMargins image
    let image' = image |> padImage top bottom |> Array.map (padRow left right)

    printIm image'
    image'
    |> Array.mapi
        (fun x row ->
            (row
             |> Array.mapi (enhancePixel algo image x)
             ))


let day20 fn () =
    let input = readInput fn
    let enchanceAlgorithm = input |> Seq.head |> Seq.map symbolToBool |> Array.ofSeq
    let enhance = enhanceImage enchanceAlgorithm
    let image =
        input
        |> Seq.skip 2
        |> Seq.map (Array.ofSeq >> Array.map symbolToBool)
        |> Array.ofSeq

    checkMargins image |> printfn "%A"
    printfn $"%d{Array.length image} x %d{Array.length (Array.head image)}"
    image |> enhance |> printIm
    let image' = image |> enhance  |> enhance 
    printIm image'
    printfn $"%d{Array.length image'} x %d{Array.length (Array.head image')}"

    image'
    |> Seq.concat
    |> Seq.sumBy
        (function
        | false -> 0
        | true -> 1)
    |> int64
