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

    let rec parseNpackets n (pkts: Packet list) bits =
        match n with
        | 1 -> pkts, bits
        | n ->
            let pkt, rest = parse [] bits
            parseNpackets (n - 1) (pkt @ pkts) rest


    match (Seq.isEmpty bits)
          || (bits |> Seq.forall (fun n -> n = 0)) with
    | true -> packets, bits
    | false ->
        let version = bits |> Seq.take 3 |> bitsToInt

        let operation =
            bits |> Seq.skip 3 |> Seq.take 3 |> bitsToInt

        let payload = bits |> Seq.skip 6

        let newPacket, rest =
            match operation with
            | 4L ->
                let value = parseValue payload []
                let skip = 6 + (Seq.length value / 4 * 5)
                let value' = bitsToInt value

                { Version = version
                  Type = operation
                  Data = Value value'
                  Length = skip },
                bits |> Seq.skip skip
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

                    let subPackets, rest = parse [] subPacketData

                    { Data = SubPackets subPackets
                      Version = version
                      Type = operation
                      Length = 6 + skip + length },
                    bits |> Seq.skip (6 + skip + length)
                | 1 ->
                    let skip = 12

                    let subPktCount =
                        Seq.tail payload
                        |> Seq.take 11
                        |> bitsToInt
                        |> int

                    let subPackets, rest =
                        parseNpackets subPktCount [] (payload |> Seq.skip skip)

                    let length =
                        6
                        + skip
                        + (subPackets |> Seq.sumBy (fun p -> p.Length))

                    { Data = SubPackets subPackets
                      Version = version
                      Type = operation
                      Length = length },
                    rest

        parse (packets @ [ newPacket ]) rest

let rec countVersions (pkt: Packet) =
    match pkt.Data with
    | SubPackets s ->
        (s |> Seq.map countVersions |> Seq.sum)
        + pkt.Version
    | Value _ -> pkt.Version


let rec getValue (pkt: Packet) =
    let boolOp fn values =
        let a = values |> Seq.head
        let b = values |> Seq.last

        match fn a b with
        | true -> 1L
        | false -> 0L

    match pkt.Type, pkt.Data with
    | 0L, SubPackets s -> s |> Seq.map getValue |> Seq.sum
    | 1L, SubPackets s -> s |> Seq.map getValue |> Seq.reduce (*)
    | 2L, SubPackets s -> s |> Seq.map getValue |> Seq.min
    | 3L, SubPackets s -> s |> Seq.map getValue |> Seq.max
    | 4L, Value n -> n
    | 5L, SubPackets s -> s |> Seq.map getValue |> boolOp (>)
    | 6L, SubPackets s -> s |> Seq.map getValue |> boolOp (<)
    | 7L, SubPackets s -> s |> Seq.map getValue |> boolOp (=)
    | _ -> failwith "Error"

let rec formatPacket pkt =
    match pkt.Type, pkt.Data with
    | 0L, SubPackets s -> sprintf "(SUM %s)" (s |> Seq.map formatPacket |> String.concat " ")
    | 1L, SubPackets s -> sprintf "(MUL %s)" (s |> Seq.map formatPacket |> String.concat " ")
    | 2L, SubPackets s -> sprintf "(MIN %s)" (s |> Seq.map formatPacket |> String.concat " ")
    | 3L, SubPackets s -> sprintf "%s" (s |> Seq.map formatPacket |> String.concat " ")
    | 4L, Value n -> $"%d{n}"
    | 5L, SubPackets s -> sprintf "(GT %s)" (s |> Seq.map formatPacket |> String.concat " ")
    | 6L, SubPackets s -> sprintf "(LT %s)" (s |> Seq.map formatPacket |> String.concat " ")
    | 7L, SubPackets s -> sprintf "(EQ %s)" (s |> Seq.map formatPacket |> String.concat " ")

let day16parse1 data =
    parse [] data |> fst |> Seq.head |> countVersions

let day16 fn () =
    let data = readInput fn |> Seq.head |> hexToBits
    day16parse1 data

let d16parse2 data =
    data
    |> hexToBits
    |> parse []
    |> fst
    |> Seq.head
    |> getValue

let day16part2 fn () =
    let packet =
        "9C0141080250320F1802104A08"
        |> hexToBits |> Seq.cache
        |> parse []
        |> fst
        |> Seq.head

    packet |> formatPacket |> printfn "%s"
    // let data = readInput fn |> Seq.concat
    // d16parse2 data
    0L
