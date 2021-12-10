module AoC2021.Day10

open AoC2021.Utils

let matchClosing =
    function
    | '<' -> '>'
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'

let score =
    function
    | ')' -> 3L
    | ']' -> 57L
    | '}' -> 1197L
    | '>' -> 25137L

let score2 =
    function
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L 
    | '>' -> 4L 
    
let opening = "({[<" |> Set.ofSeq

let rec check stack chars =
    let c = List.tryHead stack

    match chars with
    | [] -> None
    | head :: tail ->
        match c with
        | None -> check [ head ] tail
        | Some c ->
            match Set.contains head opening with
            | true -> check ([ head ] @ stack) tail
            | false ->
                match matchClosing c = head with
                | false -> Some head
                | true -> check (List.tail stack) tail
                
let rec check2 stack chars =
    let c = List.tryHead stack

    match chars with
    | [] -> Some stack
    | head :: tail ->
        match c with
        | None -> check2 [ head ] tail
        | Some c ->
            match Set.contains head opening with
            | true -> check2 ([ head ] @ stack) tail
            | false ->
                match matchClosing c = head with
                | false -> None
                | true -> check2 (List.tail stack) tail


let day10 fn () =
    let input = readInput fn |> Seq.map List.ofSeq

    input
    |> Seq.choose (check [])
    |> Seq.sumBy score

let folder tally n = tally * 5L + n

let day10part2 fn () =
    let input = readInput fn |> Seq.map List.ofSeq
    input |> Seq.filter (check [] >> function |Some _ -> false | None -> true) |> Seq.choose (check2 []) |> Seq.map (Seq.map (matchClosing >> score2) )
    |> printfn "%A"
    0L
