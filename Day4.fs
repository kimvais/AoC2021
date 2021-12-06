module AoC2021.Day4

open AoC2021.Utils

let rec doBingo f boards numbers =
    let number = Seq.head numbers

    let boards' =
        boards
        |> List.map (List.map (List.filter ((<>) number)))

    match f boards' with
    | Some l -> l, number
    | None -> doBingo f boards' (Seq.tail numbers)

let solve boardMatcher fn =
    let input = readInputDelimByEmptyLine fn

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

    let winningBoard, lastNumber = doBingo boardMatcher boards' numbers
    printfn "Winning board: %A" winningBoard

    let sumOfNumbers =
        (winningBoard |> List.sumBy List.sum) / 2 // Every number is there twice, because of transpose earlier

    sumOfNumbers * lastNumber |> int64

let day4 fn () =
    solve (List.tryFind (List.exists List.isEmpty)) fn
    
let day4part2 fn () =
    solve ((List.filter List.isEmpty) >> List.tryExactlyOne) fn