open System.IO
open System

let file = (File.ReadAllLines "input.txt").[0]

let hexToBinary =
    [ ('0', "0000")
      ('1', "0001")
      ('2', "0010")
      ('3', "0011")
      ('4', "0100")
      ('5', "0101")
      ('6', "0110")
      ('7', "0111")
      ('8', "1000")
      ('9', "1001")
      ('A', "1010")
      ('B', "1011")
      ('C', "1100")
      ('D', "1101")
      ('E', "1110")
      ('F', "1111") ]
    |> Map

let decodeHex (input: string) =
    input.ToCharArray()
    |> Array.map hexToBinary.TryFind
    |> Array.map Option.get
    |> String.Concat

let binaryString = decodeHex file

type Bit = int64
type Bits = list<Bit>

let toBits (binaryString: string) : Bits =
    let toBit (c: char) : Bit = (int c) - (int '0') |> int64

    binaryString.ToCharArray()
    |> Array.map toBit
    |> Array.toList

let inputBits = toBits binaryString

let binToInt (bits: Bits) : int64 =
    bits |> List.fold (fun acc d -> acc*2L + d) 0L

let rec readBy5 (bits: Bits) =
    match bits with
    | 1L :: a :: b :: c :: d :: tail ->
        let value, rest = readBy5 tail
        (a :: b :: c :: d :: value), rest
    | 0L :: a :: b :: c :: d :: tail -> [ a; b; c; d ], tail
    | _ -> failwith $"Unexpected input: {bits}"

type Packet =
    | Literal of int64 * int64
    | Op of int64 * int64 * List<Packet>

type LengthType =
    | L15
    | LUnknown

let readNum (n: int) (bits: Bits) : int64 * Bits =
    let num = bits |> List.take n |> binToInt
    let rest = bits |> List.skip n
    num, rest
    
let readBits (n: int64) (bits: Bits) : Bits * Bits =
    let n = n |> int 
    let taken = bits |> List.take n
    let bits = bits |> List.skip n
    taken, bits 

let rec decodePacket (bits: Bits) : Packet * Bits =
    let rec readPackets (n: int64) (bits: Bits) : List<Packet> * Bits =
        if n = 0 then
            [], bits
        else
            let packet, bits = decodePacket bits
            let packets, bits = readPackets (n - 1L) bits
            (packet :: packets), bits

    let rec readAll (bits: Bits) : List<Packet> * Bits =
        if bits |> binToInt = 0 then
            [], bits
        else
            let packet, bits = decodePacket bits
            let packets, bits = readAll bits
            (packet :: packets), bits

    let readSubs (bits: Bits) : List<Packet> * Bits =
        match bits with
        | 0L :: bits ->
            let argSize,bits = bits |> readNum 15
            let argBits, bits = bits |> readBits argSize
            let subs, _ = readAll argBits
            subs, bits
        | 1L :: bits ->
            let argCount,bits = bits |> readNum 11
            let subs, bits = bits |> readPackets argCount
            subs, bits
        | _ -> failwith $"invalid value: {bits}"

    let version = bits |> List.take 3 |> binToInt
    let bits = bits |> List.skip 3
    let typeId = bits |> List.take 3 |> binToInt
    let bits = bits |> List.skip 3

    match typeId with
    | 4L ->
        let payload, bits = readBy5 bits
        let payload = payload |> binToInt
        (Literal(version, payload)), bits
    | op ->
        let subs, bits = readSubs bits
        (Op(version, op, subs)), bits

let structure, _ = decodePacket inputBits

let rec count (packet: Packet) =
    match packet with
    | Literal (version, num) -> version
    | Op (version, op, subs) -> version + (subs |> List.map count |> List.sum)

let task1 = count structure

printfn $"Task 1: {task1}"

let rec eval (packet: Packet) : int64 =
    match packet with
    | Literal (_, num) -> num |> int64
    | Op (_, 0L, subs) -> subs |> List.map eval |> List.sum
    | Op (_, 1L, subs) -> subs |> List.map eval |> List.reduce (*)
    | Op (_, 2L, subs) -> subs |> List.map eval |> List.min
    | Op (_, 3L, subs) -> subs |> List.map eval |> List.max
    | Op (_, 5L, subs) ->
        let s1 = eval subs.Head
        let s2 = eval subs.Tail.Head
        if s1 > s2 then 1L else 0L
    | Op (_, 6L, subs) ->
        let s1 = eval subs.Head
        let s2 = eval subs.Tail.Head
        if s1 < s2 then 1L else 0L
    | Op (_, 7L, subs) ->
        let s1 = eval subs.Head
        let s2 = eval subs.Tail.Head
        if s1 = s2 then 1L else 0L

let task2 = eval structure
printfn $"Task 2: {task2}"
