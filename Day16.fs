module AoC2021.Day16

open AoC2021.Utils

type Packet =
    | DataPacket
    | OperatorPacket

type DataPacket =
    { Version: int64
      Type: int64
      Value: int64 }

type OperatorPacket =
    { Version: int64
      Type: int64
      SubPackets: Packet list }

let rec parseValue bits results =
    let chunk = bits |> Seq.take 5 |> List.ofSeq
    match chunk with
    | 0 :: tail -> results @ tail
    | 1 :: tail -> parseValue (bits |> Seq.skip 5) (results @ tail)
        
    
let rec parse bits =
    printfn "%A" bits
    let version = bits |> Seq.take 3 |> bitsToInt
    let typ = bits |> Seq.skip 3 |> Seq.take 3 |> bitsToInt
    let payload = bits |> Seq.skip 6
    let value = parseValue payload [] |> bitsToInt
    { Version=version; Type=typ; Value=value}

let day16 fn () =
    hexToBits "D2FE28" |> parse |> printfn "%A"
    0L
