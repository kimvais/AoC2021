module AoC2021.Day24

open AoC2021.Utils

type Register =
    | W
    | X
    | Y
    | Z

type Input = { Data: int64 array; Ptr: int }

type Argument =
    | N of int64
    | R of Register

let stor (mem: int64 array) r v =
    match r with
    | W -> mem.[0] <- v
    | X -> mem.[1] <- v
    | Y -> mem.[2] <- v
    | Z -> mem.[3] <- v

let load (mem: int64 array) r =
    match r with
    | W -> mem.[0]
    | X -> mem.[1]
    | Y -> mem.[2]
    | Z -> mem.[3]

let inp mem r data =
    stor mem r data.Data.[data.Ptr]
    { data with Ptr = data.Ptr + 1 }

let operation op mem r arg =
    let v =
        match arg with
        | N n -> n
        | R r' -> load mem r'

    stor mem r (op (load mem r) v)

let add = operation (+)
let mul = operation (*)
let div = operation (/)
let mod' = operation (%)

let equals a b =
    match a = b with
    | false -> 0L
    | true -> 1L

let eql = operation equals

let day24 fn () =
    let input = readInput fn

    let serialNumbers =
        Seq.unfold
            (fun i ->
                if i <= 11_111_111_111_111L then
                    None
                else
                    Some(string i |> Seq.map charToL |> List.ofSeq, i - 1L))
            100_000_000_000_000L
        |> Seq.filter (List.contains 0L >> not)

    0L
