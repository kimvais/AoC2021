module AoC2021.Day21

open AoC2021.Utils
open Microsoft.FSharp.Core

type Player =
    | One
    | Two

type Game =
    { Dice: int
      Pos1: int
      Pos2: int
      Score1: int
      Score2: int
      Rolls: int }

type QuantumGame =
    {
        Universes: int list list
        Player1Turn: bool
        Pos1: int
        Pos2: int
        Score1: int
        Score2: int
        P1Wins: int64
        P2Wins: int64
    }
let roll3 d =
    let numbers =
        [ d + 3; d; d + 1; d + 2 ]
        |> Seq.map (fun n -> (n - 1) % 100 + 1)

    Seq.head numbers, Seq.sum (Seq.tail numbers)

let universes =
        cartesian [ [ 1; 2; 3 ]
                    [ 1; 2; 3 ]
                    [ 1; 2; 3 ] ]
        |> List.sort |> List.rev

let rec play game =
    let rolls = game.Rolls + 3
    let dice, roll = roll3 game.Dice
    let player = (rolls / 3) % 2

    let score, game' =
        match player with
        | 1 ->
            let pos = (game.Pos1 + roll - 1) % 10 + 1
            let score = (game.Score1 + pos)

            score,
            { game with
                  Dice = dice
                  Pos1 = pos
                  Score1 = score
                  Rolls = rolls }
        | 0 ->
            let pos = (game.Pos2 + roll - 1) % 10 + 1
            let score = (game.Score2 + pos)

            score,
            { game with
                  Dice = dice
                  Pos2 = pos
                  Score2 = score
                  Rolls = rolls }

    // printfn "Player %d rolls %d for a total score of %d (dice now at %d)" (player + 1) roll score dice

    match score with
    | s when s >= 1000 -> game'
    | _ -> play game'

let printEveryN = 1000000L
let rec playQuantum game =
    match game.P1Wins % printEveryN, game.P2Wins % printEveryN with
    | 1L,_  -> printfn "Player one has won %d times" game.P1Wins
    | _, 1L -> printfn "Player two has won %d times" game.P2Wins
    | _ -> ()
    let head = List.head universes
    let tail = List.tail universes
    let roll = Seq.sum head
    let score, game' =
        match game.Player1Turn with
        | true ->
            let pos = (game.Pos1 + roll - 1) % 10 + 1
            let score = (game.Score1 + pos)
            score, {game with Pos1=pos; Score1=score; Universes=tail; Player1Turn = false}
        | false ->
            let pos = (game.Pos2 + roll - 1) % 10 + 1
            let score = (game.Score2 + pos)
            score, {game with Pos1=pos; Score1=score; Universes=tail; Player1Turn = true}
    match score with
    | n when n >= 21 ->
        match game.Player1Turn with
            | true ->
                let p1, p2 = playQuantum {game' with Universes=universes}
                (game'.P1Wins + 1L + p1), (game'.P2Wins + p2)
            | false ->
                let p1, p2 = playQuantum {game' with Universes=universes}
                game'.P1Wins + p1, (game'.P2Wins + 1L + p2)
    | _ -> playQuantum game'
    
    
let day21 sp1 sp2 () =
    let start =
        { Dice = 1
          Pos1 = sp1
          Pos2 = sp2
          Score1 = 0
          Score2 = 0
          Rolls = 0 }

    let game = play start
    let losingscore = List.min [ game.Score1; game.Score2 ] |> int64
    (int64 game.Rolls) * losingscore

let day21part2 sp1 sp2 () =
    let p1, p2 = playQuantum {
        Universes=universes
        Player1Turn=true
        Pos1=sp1
        Pos2=sp2
        Score1=0
        Score2=0
        P1Wins=0
        P2Wins=0
    }
    printfn "P1: %d" p1
    printfn "P2: %d" p2
    0L
