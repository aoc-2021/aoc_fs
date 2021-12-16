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

type Bit = int 
type Bits = list<Bit>
let toBits (binaryString:string): Bits = binaryString.ToCharArray () |> Array.map (fun c -> (int c) - (int '0')) |> Array.toList
let bits = toBits binaryString

printfn $"{binaryString}"
printfn $"{bits}"

let binToInt (bits:Bits) =
   let rec multUp (value:int) (bits:Bits) =
        match bits with
        | [] -> value
        | a::tail -> multUp (value*2+a) tail
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
    | 1::a::b::c::d::tail ->
        let (value,rest) = readBy5 tail
        (a::b::c::d::value),tail
    | 0::a::b::c::d::tail ->
        [a;b;c;d],tail
    | _ -> failwith $"Unexpected input: {bits}"    

type Packet =
    | Literal of int*int
    | Op6 of int*List<Packet>
    | Op3 of int*List<Packet>
    | PUnknown of int*int 

type LengthType =
    | L15
    | LUnknown

let readNum (bits:Bits) =
    match bits with
    | 0::tail ->
        let num = tail |> List.take 15 |> binToInt
        let tail = tail |> List.skip 15
        num,tail 
    | 1::tail ->
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
        
    let version = bits |> List.take 3 |> binToInt 
    let bits = bits |> List.skip 3 
    let typeId = bits |> List.take 3 |> binToInt 
    let bits = bits |> List.skip 3
    printfn $"{version}"
    printfn $"{typeId}"
    match typeId with
    | 4 -> 
        let payload,bits = readBy5 bits
        let payload = payload |> binToInt
        (Literal (version,payload)),bits
    | 6 ->
        let subLength,bits = readNum bits
        printfn $"subLength: {subLength}"
        let subs = bits |> List.take subLength
        let package1,subs = decodePacket subs
        let package2,subs = decodePacket subs
        (Op6 (version,[package1;package2])),bits
    | 3 ->
        let subPackages,bits = readNum bits
        printfn $"subPackages: {subPackages}"
        let subs,bits = readPackets subPackages bits 
        // let package2,subs = decodePacket subs
        (Op3 (version,subs)),bits
    | _ -> (PUnknown (version,typeId)),bits

decodePacket bits

let package = decodePacket ("110100101111111000101000" |> toBits)
printfn $"package = {package}"

let package2 = decodePacket ("00111000000000000110111101000101001010010001001000000000" |> toBits)
printfn $"package2 = {package2}"

let package3 = decodePacket ("11101110000000001101010000001100100000100011000001100000" |> toBits)
printfn $"package3 = {package3}"
