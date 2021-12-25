module AoC2021.Day25

open AoC2021.Utils

let getNext (arr: 'a array) i = arr.[(i + 1) % Array.length arr]

let getPrev (arr: 'a array) i =
    match Array.tryItem (i - 1) arr with
    | None -> arr.[Array.length arr - 1]
    | Some c -> c

let moveRowEast row =
    row
    |> Array.mapi
        (fun i c ->
            let n = getNext row i
            let p = getPrev row i

            match c, p, n with
            | '.', '>', _ -> '>'
            | '>', _, '.' -> '.'
            | x, _, _ -> x)

let moveRowSouth row =
    row
    |> Array.mapi
        (fun i c ->
            let n = getNext row i
            let p = getPrev row i

            match c, p, n with
            | '.', 'v', _ -> 'v'
            | 'v', _, '.' -> '.'
            | x, _, _ -> x)

let moveEast map = map |> Array.map moveRowEast

let moveSouth map =
    map
    |> Array.transpose
    |> Array.map moveRowSouth
    |> Array.transpose

let printMap map =
    printfn ""
    let printRow row = row |> Array.map string |> String.concat "" |> printfn "%s"
    map |> Array.iter printRow

let moveCucumbers = moveEast >> moveSouth

let rec move map i =
    let map' = moveCucumbers map

    match map = map' with
    | true -> i + 1L
    | false -> move map' i + 1L

let day25 fn () =
    let input = readInput fn |> Seq.map Array.ofSeq |> Array.ofSeq
    move input 0L
