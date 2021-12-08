module AoC2021.Day8

open AoC2021.Utils

let isOneFourSevenOrEight s =
    match String.length s with
    | 2 | 3| 4| 7 -> true
    | _ ->  false
   
let getNotes input =
    input |> Seq.map (Seq.last >> split ' ' >> Seq.map (fun s -> s.Trim())) |> Seq.concat

let getInput fn =    
    readInput fn |> Seq.map (splitS " \| ")
let day8 fn () =
    let input = getInput fn
    let notes = getNotes input
    printfn "%A" (notes |> List.ofSeq)
    let onesFoursSevensAndEights = notes |> Seq.filter isOneFourSevenOrEight
    printfn "%A" onesFoursSevensAndEights
    onesFoursSevensAndEights |> Seq.length |> int64
    
let day8part2 fn () =
    let input = getInput fn
    let notes = getNotes input
    let candidates = notes |> Set.ofSeq
    printfn "%A" candidates
    0L
