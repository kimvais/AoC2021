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

let rec countVersions (pkt:Packet) =
    match pkt.Data with
    | SubPackets s -> (s |> Seq.map countVersions |> Seq.sum) + pkt.Version
    | Value _ -> pkt.Version

let rec getValue (pkt:Packet)  =
    match pkt.Type, pkt.Data with
    | 0L, SubPackets s -> s |> Seq.map getValue |> Seq.sum
    | 1L, SubPackets s -> s|> Seq.map getValue |> Seq.reduce (*)
    | 2L, SubPackets s -> s |> Seq.map getValue |> Seq.min
    | 3L, SubPackets s -> s |> Seq.map getValue |> Seq.max
    | 4L, Value n -> n
    | 5L, SubPackets s -> match (s |> Seq.head |> getValue) > (s |> Seq.last |> getValue) with | true -> 1L | false -> 0L
    | 6L, SubPackets s -> match (s |> Seq.head |> getValue) < (s |> Seq.last |> getValue) with | true -> 1L | false -> 0L
    | 7L, SubPackets s -> match (s |> Seq.head |> getValue) = (s |> Seq.last |> getValue) with | true -> 1L | false -> 0L
    | _ -> failwith "Error"
     
let day16 fn () =
    let data = readInput fn  |> Seq.head |> hexToBits
    parse [] data |> Seq.head |> countVersions 

let d16parse2 data =
    data |> hexToBits |> parse [] |> Seq.head |> getValue
    
let day16part2 fn () =
    let data = readInput fn |> Seq.concat
    d16parse2 data
