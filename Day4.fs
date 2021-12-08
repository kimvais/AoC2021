module AoC2021.Day4

open AoC2021.Utils

let rec doBingo boards numbers =
    let number = Seq.head numbers

    let boards' =
        boards
        |> List.map (List.map (List.filter ((<>) number)))

    match List.tryFind (List.exists List.isEmpty) boards' with
    | Some l -> l, number, (boards' |> List.filter (List.exists List.isEmpty)), Seq.tail numbers
    | None -> doBingo boards' (Seq.tail numbers)

let parseInput input =
    let numbers =
        Seq.head input |> split ',' |> Seq.map int

    let splitFilterAndMakeNumber =
        (split ' '
         >> List.ofArray
         >> List.filter (String.length >> (<) 0)
         >> List.map int)

    let boards =
        Seq.tail input
        |> List.ofSeq
        |> List.map (
            splitByLinefeed
            >> List.ofSeq
            >> List.map splitFilterAndMakeNumber
        )

    let boards' =
        boards |> List.map (fun l -> l @ List.transpose l)

    boards', numbers

let solve fn =
    let input = readInputDelimByEmptyLine fn
    let boards, numbers = parseInput input
    doBingo boards numbers


let day4 fn () =
    let winningBoard, lastNumber, _, _ = solve fn

    let sumOfNumbers =
        (winningBoard |> List.sumBy List.sum) / 2 // Every number is there twice, because of transpose earlier

    sumOfNumbers * lastNumber |> int64

let rec part2 remaining numbers =
    match Seq.length remaining with
    | 1 -> doBingo remaining numbers
    | _ ->
        let _, _, remaining', numbers' = doBingo remaining numbers
        part2 remaining' numbers'

let day4part2 fn () =
    let input = readInputDelimByEmptyLine fn
    let boards, numbers = parseInput input
    let _, _, lastBoard, numbers = part2 boards numbers
    let winningBoard, lastNumber,_ ,_  = doBingo lastBoard numbers

    let sumOfNumbers =
        (winningBoard |> List.sumBy List.sum) / 2 // Every number is there twice, because of transpose earlier

    sumOfNumbers * lastNumber |> int64
