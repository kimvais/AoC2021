module AoC2021.Day2

open AoC2021.Utils

type Position = {
    X: int
    Depth: int
    Aim: int
}

let move state instruction =
    match instruction with
    | [|"forward";n|] -> {state with X=state.X + int n}
    | [|"down";n|] -> {state with Depth=state.Depth + int n}
    | [|"up";n|] -> {state with Depth=state.Depth - int n}
    
let move2 state instruction =
    match instruction with
    | [|"forward";n|] -> {state with X=state.X + int n; Depth=state.Depth + state.Aim * int n}
    | [|"down";n|] -> {state with Aim=state.Aim + int n}
    | [|"up";n|] -> {state with Aim=state.Aim - int n}
    
    
let day2 fn () =
    let input = readInput fn |> Seq.map (fun (s:string) -> s.Split ' ')
    let finalPosition = input |> Seq.fold move {X=0; Depth=0; Aim=0}
    finalPosition.X * finalPosition.Depth
   
let day2part2 fn ()  =
    let input = readInput fn |> Seq.map (fun (s:string) -> s.Split ' ')
    let finalPosition = input |> Seq.fold move2 {X=0; Depth=0; Aim=0}
    finalPosition.X * finalPosition.Depth
    