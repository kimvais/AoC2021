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

let rec parse packets bits =
    match (Seq.isEmpty bits) || (bits |> Seq.forall (fun n -> n = 0)) with
    | true -> packets
    | false -> 
        // match thereIsMore bits with
        let version = bits |> Seq.take 3 |> bitsToInt

        let typ =
            bits |> Seq.skip 3 |> Seq.take 3 |> bitsToInt

        let payload = bits |> Seq.skip 6

        let newPacket, rest =
            match typ with
            | 4L ->
                let value = parseValue payload []
                let skip = 6 + (Seq.length value / 4 * 5)
                let value' = bitsToInt value

                { Version = version
                  Type = typ
                  Data = Value value'
                  Length = skip }, bits |> Seq.skip skip
            | _ ->
                let lengthType = Seq.head payload

                match lengthType with
                | 0 ->
                    let skip = 16
                    let length =
                        Seq.tail payload
                        |> Seq.take 15
                        |> bitsToInt
                        |> int

                    let subPacketData =
                        payload |> Seq.skip skip |> Seq.take length

                    let subPackets = parse [] subPacketData

                    { Data = SubPackets subPackets
                      Version = version
                      Type = typ
                      Length = 6 + skip + length }, bits |> Seq.skip (6 + skip + length)
                | 1 ->
                    let skip = 12
                    let subPktCount =
                        Seq.tail payload
                        |> Seq.take 11
                        |> bitsToInt
                        |> int
                    let subPackets =
                        parse [] (payload |> Seq.skip skip)
                        // parseNpackets (subPktCount,(payload |> Seq.skip 12),[])

                    let length =
                        6
                        + skip
                        + (subPackets |> Seq.sumBy (fun p -> p.Length))

                    { Data = SubPackets subPackets
                      Version = version
                      Type = typ
                      Length = length }, bits |> Seq.skip length

        parse (packets @ [newPacket]) rest

let day16 fn () =
    hexToBits "D2FE28" |> parse [] |> printfn "1: %A"

    hexToBits "38006F45291200" |> parse [] |> printfn "2: %A"

    hexToBits "EE00D40C823060" |> parse [] |> printfn "3: %A"

    hexToBits "8A004A801A8002F478" |> parse [] |> printfn "4: %A"

    hexToBits "620080001611562C8802118E34" |> parse [] |> printfn "5: %A"
    hexToBits "C0015000016115A2E0802F182340" |> parse [] |> printfn "6: %A"
    hexToBits "A0016C880162017C3686B18A3D4780" |> parse [] |> printfn "7: %A"
    0L
