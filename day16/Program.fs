open System.IO
open System
open System.Runtime.InteropServices 

let file = (File.ReadAllLines "input.txt").[0]

let hexToBinary =
    [
        ('0',"0000")
        ('1',"0001")
        ('2',"0010")
        ('3',"0011")
        ('4',"0100")
        ('5',"0101")
        ('6',"0110")
        ('7',"0111")
        ('8',"1000")
        ('9',"1001")
        ('A',"1010")
        ('B',"1011")
        ('C',"1100")
        ('D',"1101")
        ('E',"1110")
        ('F',"1111")
    ] |> Map
    
let decode (input:string) = input.ToCharArray ()
                            |> Array.map hexToBinary.TryFind
                            |> Array.map Option.get
                            |> String.Concat
let binaryString = decode file

type Bit = int64
type Bits = list<Bit>
let toBits (binaryString:string): Bits = binaryString.ToCharArray () |> Array.map (fun c -> ((int c) - (int '0')) |> int64) |> Array.toList
let bits = toBits binaryString

printfn $"{binaryString}"
printfn $"{bits}"

let binToInt (bits:Bits) : int64 =
   let rec multUp (value:int64) (bits:Bits) =
        match bits with
        | [] -> value
        | a::tail -> multUp (value*2L+a) tail
   multUp 0 bits 

let rec padTo4 (bits:Bits) =
    if bits.Length % 4 = 0 then bits
    else padTo4 (0::bits)
    
let rec splitBy4 (bits:Bits) =
    match bits with
    | [] -> []
    | a::b::c::d::tail -> [a;b;c;d]::(splitBy4 tail)

let rec readBy5 (bits:Bits) =
    match bits with
    | 1L::a::b::c::d::tail ->
        let (value,rest) = readBy5 tail
        (a::b::c::d::value),rest
    | 0L::a::b::c::d::tail ->
        [a;b;c;d],tail
    | _ -> failwith $"Unexpected input: {bits}"    

type Packet =
    | Literal of int64*int64
    | Op of int64*int64*List<Packet>

type LengthType =
    | L15
    | LUnknown

let readNum (bits:Bits) =
    match bits with
    | 0L::tail ->
        let num = tail |> List.take 15 |> binToInt
        let tail = tail |> List.skip 15
        num,tail 
    | 1L::tail ->
        let num = tail |> List.take 11 |> binToInt
        let tail = tail |> List.skip 11
        num,tail 
    | _ -> failwith $"not a number: {bits}"

let rec decodePacket (bits:Bits) : Packet*Bits =
    let rec readPackets (n:int) (bits:Bits) : List<Packet>*Bits =
        if n = 0 then [],bits
        else
            let packet,bits = decodePacket bits 
            let packets,bits = readPackets (n-1) bits 
            (packet::packets),bits  
    let rec readAll (bits:Bits) : List<Packet>*Bits =
        printfn $"readAll: {bits.Length}"
        if bits |> binToInt = 0 then [],bits
        else
            let packet,bits = decodePacket bits
            let packets,bits = readAll bits
            (packet::packets),bits
    let readSubs (bits:Bits) : List<Packet>*Bits =
        match bits with
        | 0L::bits ->
            let argSize = bits |> List.take 15 |> binToInt |> int 
            let bits = bits |> List.skip 15
            let argBits = bits |> List.take argSize
            let bits = bits |> List.skip argSize
            let subs,_ = readAll argBits
            subs,bits
        | 1L::bits ->
            let argCount = bits |> List.take 11 |> binToInt |> int 
            let bits = bits |> List.skip 11
            let subs,bits = readPackets argCount bits
            subs,bits
        | _ -> failwith $"invalid value: {bits}"
    let version = bits |> List.take 3 |> binToInt 
    let bits = bits |> List.skip 3 
    let typeId = bits |> List.take 3 |> binToInt 
    let bits = bits |> List.skip 3
    printfn $"{version}"
    printfn $"{typeId}"
    match typeId with
    | 4L -> 
        let payload,bits = readBy5 bits
        let payload = payload |> binToInt
        (Literal (version,payload)),bits
    | op ->
        let subs,bits = readSubs bits 
        (Op (version,op,subs)),bits

// decodePacket bits

let package = decodePacket ("110100101111111000101000" |> toBits)
printfn $"package = {package}"

let package2 = decodePacket ("00111000000000000110111101000101001010010001001000000000" |> toBits)
printfn $"package2 = {package2}"

let package3 = decodePacket ("11101110000000001101010000001100100000100011000001100000" |> toBits)
printfn $"package3 = {package3}"

let package4 = decodePacket ("8A004A801A8002F478" |> decode |> toBits)
printfn $"package4 = {package4}"

let structure,_= decodePacket bits

let rec count (packet:Packet) =
    match packet with
        | Literal (version,num) -> version
        | Op (version,op,subs) -> version + (subs |> List.map count |> List.sum )
        
let task1 = count structure

printfn $"task1: {task1}"

let rec eval (packet:Packet) : int64 =
    match packet with
    | Literal(_,num) -> num |> int64
    | Op(_,0L,subs) -> subs |> List.map eval |> List.sum 
    | Op(_,1L,subs) -> subs |> List.map eval |> List.reduce (*)
    | Op(_,2L,subs) -> subs |> List.map eval |> List.min
    | Op(_,3L,subs) ->subs |> List.map eval |> List.max 
    | Op(_,5L,subs) ->
        let s1 = eval subs.Head
        let s2 = eval subs.Tail.Head
        if s1 > s2 then 1L else 0L
    | Op(_,6L,subs) ->
        let s1 = eval subs.Head
        let s2 = eval subs.Tail.Head
        if s1 < s2 then 1L else 0L
    | Op(_,7L,subs) ->
        let s1 = eval subs.Head
        let s2 = eval subs.Tail.Head
        if s1 = s2 then 1L else 0L

let task2 = eval structure
printfn $"task2 {task2}"

let evals str =
    let a = str
            |> decode |> toBits 
            |> decodePacket
            |> fst
            |> eval 
    printf $"{str} -> {a}"
    a

evals "880086C3E88112"

List.re