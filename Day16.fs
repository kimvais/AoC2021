module AoC2021.Day16

open AoC2021.Utils

type Payload =
    | Value of int64
    | SubPackets of Packet list

and Packet =
    { Version: int64
      Type: int64
      Length: int
      Data: Payload }

let rec parseValue bits results =
    let chunk = bits |> Seq.take 5 |> List.ofSeq

    match chunk with
    | 0 :: tail -> results @ tail
    | 1 :: tail -> parseValue (bits |> Seq.skip 5) (results @ tail)


let rec parse packets packetCount bits =
    printfn "%A" bits
    // match thereIsMore bits with
    let version = bits |> Seq.take 3 |> bitsToInt

    let typ =
        bits |> Seq.skip 3 |> Seq.take 3 |> bitsToInt

    let payload = bits |> Seq.skip 6

    let newPacket =
        match typ with
        | 4L ->
            let value = parseValue payload []
            printfn $"%A{value}"
            let skip = 6 + (Seq.length value / 4 * 5)
            let value' = bitsToInt value

            { Version = version
              Type = typ
              Data = Value value'
              Length = skip }
        | _ ->
            let lengthType = Seq.head payload

            match lengthType with
            | 0 ->
                let length =
                    Seq.tail payload
                    |> Seq.take 15
                    |> bitsToInt
                    |> int

                let subPacketData =
                    payload |> Seq.skip 16 |> Seq.take length

                let subPackets = parse [] -1 subPacketData 

                { Data = SubPackets subPackets
                  Version = version
                  Type = typ
                  Length = 6 + 16 + length }
            | 1 ->
                let subPktCount = Seq.tail payload |> Seq.take 11 |> bitsToInt |> int
                let subPackets = parse [] subPktCount (payload |> Seq.skip 12)
                let length = 6+12 + (subPackets |> Seq.sumBy (fun p -> p.Length))
                { Data = SubPackets subPackets
                  Version = version
                  Type = typ
                  Length = length  }
                                     

    let tail = bits |> Seq.skip newPacket.Length
    let newPackets = packets @ [ newPacket ]

    match Seq.length newPackets = packetCount || Seq.isEmpty tail
          || tail |> Seq.forall (fun n -> n = 0) with
    | true -> newPackets
    | false -> parse newPackets -1 tail

let day16 fn () =
    hexToBits "D2FE28" |> parse  [] -1 |> printfn "%A"
    hexToBits "38006F45291200" |> parse [] -1 |> printfn "%A"
    hexToBits "EE00D40C823060" |> parse [] -1 |> printfn "%A"
    // hexToBits "8A004A801A8002F478A" |> parse [] |> printfn "%A"
    // hexToBits "620080001611562C8802118E34" |> parse [] -1|> printfn "%A"
    0L
