module AoC2021.Day11

open AoC2021.Utils

type Octopus = { Energy: int; Flashed: bool }

let getNeighBours x y =
    seq [ x - 1, y - 1
          x - 1, y
          x - 1, y + 1
          x, y - 1
          x, y + 1
          x + 1, y - 1
          x + 1, y
          x + 1, y + 1 ]
    |> Seq.filter (fun (x, y) -> x >= 0 && y >= 0 && x < 10 && y < 10)

let rec flash (octopi: Octopus [] []) x y =
    let octopus =
        { octopi.[x].[y] with
              Energy = octopi.[x].[y].Energy + 1 }

    match octopus.Flashed with
    | true -> ()
    | false ->
        match octopus.Energy with
        | 10 ->
            octopi.[x].[y] <- { Energy = 0; Flashed = true }

            getNeighBours x y
            |> Seq.iter (fun (x, y) -> flash octopi x y)

        | _ -> octopi.[x].[y] <- octopus

let presentOctopus o =
    match o.Flashed with
    | true -> "0"
    | false -> o.Energy |> string

let printRow row =
    row
    |> Array.map presentOctopus
    |> String.concat ""
    |> printfn "%s"

let printOctopi (octopi: Octopus [] []) =
    octopi |> Array.iter (fun row -> printRow row)
    printfn ""

let day11 fn rounds () =
    let octopi =
        readInput fn
        |> Array.ofSeq
        |> Array.map (
            Array.ofSeq
            >> Array.map (
                charToInt
                >> (fun e -> { Energy = e; Flashed = false })
            )
        )

    let doRoundRow x row =
        row |> Array.iteri (fun y _ -> flash octopi x y)

    let resetOne (octopi: Octopus [] []) x y =
        let octopus = octopi.[x].[y]
        octopi.[x].[y] <- { octopus with Flashed = false }

    let resetRow x row =
        row
        |> Array.iteri (fun y _ -> resetOne octopi x y)

    // printOctopi octopi

    let countFlashes octopi =
        octopi
        |> Array.map (
            Array.map
                (fun o ->
                    match o.Energy with
                    | 0 -> 1
                    | _ -> 0)
            >> Array.sum
        )
        |> Array.sum

    let resetAll octopi = octopi |> Array.iteri resetRow

    let flashes =
        [ 0 .. (rounds - 1) ]
        |> Seq.map
            (fun _ ->
                octopi |> Array.iteri doRoundRow
                let flashes = octopi |> countFlashes
                octopi |> resetAll
                flashes)
        |> Seq.sum
        |> int64

    // printOctopi octopi
    flashes
