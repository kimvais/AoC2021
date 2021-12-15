module AoC2021.Day15

open AoC2021.Utils

type State = { X: int; Y: int; Risk: int64 }

let rec traverse grid state =
    let maxX = Array.length grid - 1
    let maxY = Array.length (Array.head grid) - 1
    seq [ match state.X, state.Y with
          | x, y when x = maxX && y = maxY -> yield state.Risk 
          | x, y when x = maxX -> yield! traverse grid {state with Y = state.Y + 1; Risk = state.Risk + grid.[x].[y + 1]}
          | x, y when y = maxY -> 
              yield! traverse grid {state with X = state.X + 1; Risk = state.Risk + grid.[x + 1].[y]}
          | x, y ->
              yield! traverse grid {state with Y = state.Y + 1; Risk = state.Risk + grid.[x].[y + 1]}
              yield! traverse grid {state with X = state.X + 1; Risk = state.Risk + grid.[x + 1].[y]}
    ]
let day15 fn () =
    let grid =
        readInput fn
        |> Seq.map (Seq.map (charToInt >> int64) >> Array.ofSeq )
        |> Array.ofSeq

    traverse grid {X=0; Y=0; Risk=0L} |> Seq.min
