module AoC2021.Day10

open AoC2021.Utils

let matchClosing =
    function
    | '<' -> '>'
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'

let score c =
    function
    | ')' -> 3L
    | ']' -> 57L
    | '}' -> 1197L
    | '>' -> 25137L

let opening = "({[<" |> Set.ofSeq

let rec check stack chars =
    let c = List.tryHead stack

    match chars with
    | [] -> None
    | head :: tail ->
        match c with
        | None -> check [head] tail
        | Some c ->
            match Set.contains c opening with
            | true -> check ([ c ] @ stack) tail
            | false ->
                match matchClosing c = head with
                | false -> Some head
                | true -> check (List.tail stack) tail

let day10 fn () =
    let input = readInput fn |> Seq.map List.ofSeq
    // input |> Seq.choose (check [])|> printfn "%A"
    input |> Seq.skip 2 |> Seq.head  |> check [] |> printfn "%A"

    0L

let day10part2 fn () =
    let input = readInput fn |> Seq.map List.ofSeq
    0L