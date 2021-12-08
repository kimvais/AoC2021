module AoC2021.Day8

open AoC2021.Utils

let isOneFourSevenOrEight s =
    match String.length s with
    | 2
    | 3
    | 4
    | 7 -> true
    | _ -> false

let getNotes input =
    input
    |> Seq.map (
        Seq.last
        >> split ' '
        >> Seq.map (fun s -> s.Trim())
    )

let getRawData line =
    line
    |> Seq.map (
        Seq.head
        >> split ' '
        >> Seq.map (fun s -> s.Trim())
    )

let getInput fn = readInput fn |> Seq.map (splitS " \| ")

let day8 fn () =
    let input = getInput fn
    let notes = getNotes input |> Seq.concat
    printfn "%A" (notes |> List.ofSeq)

    let onesFoursSevensAndEights =
        notes |> Seq.filter isOneFourSevenOrEight

    printfn "%A" onesFoursSevensAndEights
    onesFoursSevensAndEights |> Seq.length |> int64

let getDigits notes =
    let candidates = notes |> Seq.map Set.ofSeq |> Set.ofSeq

    let eight =
        notes
        |> Seq.find (fun s -> String.length s = 7)
        |> Set.ofSeq

    let seven =
        notes
        |> Seq.find (fun s -> String.length s = 3)
        |> Set.ofSeq

    let four =
        notes
        |> Seq.find (fun s -> String.length s = 4)
        |> Set.ofSeq

    let one =
        notes
        |> Seq.find (fun s -> String.length s = 2)
        |> Set.ofSeq

    let known =
        [ eight; seven; four; one ]
        |> Seq.map Set.ofSeq
        |> Set.ofSeq

    let candidates' = (Set.difference candidates known)

    let six =
        candidates'
        |> Seq.filter (fun s -> Set.count s = 6 && (not (Set.isSubset one s)))
        |> Seq.exactlyOne

    let candidates'' = Set.remove six candidates'

    let nine =
        candidates''
        |> Seq.filter (fun s -> Set.count s = 6 && Set.isSubset four s)
        |> Seq.exactlyOne

    let candidates''' = Set.remove nine candidates''

    let zero =
        candidates'''
        |> Seq.filter (fun s -> Set.count s = 6)
        |> Seq.exactlyOne

    let candidates'''' = Set.remove zero candidates'''

    let three =
        candidates''''
        |> Seq.filter (fun s -> Set.isSubset one s)
        |> Seq.exactlyOne

    let candidates''''' = Set.remove three candidates''''

    let five =
        candidates'''''
        |> Seq.filter (fun s -> Set.isSubset s nine)
        |> Seq.exactlyOne

    let two =
        Set.remove three (Set.remove five candidates''''')
        |> Seq.exactlyOne

    [ zero
      one
      two
      three
      four
      five
      six
      seven
      eight
      nine ]

let day8part2 fn () =
    let input = getInput fn
    Seq.map input (fun line ->
        let data = getRawData line
        let digits = data |> Seq.iteri (fun (i, d) -> d, i) |> Map.ofSeq
        let numbers = getNotes line |> Seq.map Set.ofSeq
    )
    0
