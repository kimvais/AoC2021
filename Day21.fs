module AoC2021.Day21

open AoC2021.Utils

let repeating max startPos = Seq.initInfinite (fun i -> (i + startPos - 1) % max + 1)

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

let roll3 d =
    let numbers =
        [ d + 3; d; d + 1; d + 2 ]
        |> Seq.map (fun n -> (n - 1) % 100 + 1)

    Seq.head numbers, Seq.sum (Seq.tail numbers)

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
    let universes =
        cartesian [ [ 1; 2; 3 ]
                    [ 1; 2; 3 ]
                    [ 1; 2; 3 ] ]
        |> List.sort

    0L
