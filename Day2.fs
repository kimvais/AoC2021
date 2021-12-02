module AoC2021.Day2

open AoC2021.Utils

type Position = { X: int; Depth: int; Aim: int }

let move position instruction =
    match instruction with
    | [| "forward"; n |] -> { position with X = position.X + int n }
    | [| "down"; n |] ->
        { position with
              Depth = position.Depth + int n }
    | [| "up"; n |] ->
        { position with
              Depth = position.Depth - int n }

let move2 position instruction =
    match instruction with
    | [| "forward"; n |] ->
        { position with
              X = position.X + int n
              Depth = position.Depth + position.Aim * int n }
    | [| "down"; n |] ->
        { position with
              Aim = position.Aim + int n }
    | [| "up"; n |] ->
        { position with
              Aim = position.Aim - int n }

let solve mover fn =
    let input =
        readInput fn
        |> Seq.map (fun (s: string) -> s.Split ' ')

    let finalPosition =
        input
        |> Seq.fold mover { X = 0; Depth = 0; Aim = 0 }

    finalPosition.X * finalPosition.Depth

let day2 fn () = solve move fn
let day2part2 fn () = solve move2 fn
