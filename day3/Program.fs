

open System
open System.IO

let readFile (fileName: String) = seq {
   use sr = new StreamReader(fileName)
   while not sr.EndOfStream do
      yield sr.ReadLine ()
}

let lines = readFile "input.txt" |> Seq.toArray

let digs (line:String) : array<char> =
    let chars = line.ToCharArray () 
    chars 

let toSign (i:char) =
    if i = '1' then 1L
    else if i = '0' then -1L
    else i |> int64 

let adds (line:String) =
    let chars = digs line
    let ints = chars |> Array.map toSign
    ints    

let adders = lines |> Seq.map adds 

let dig1 = adders |> Seq.map (fun x -> x.[0]) |> Seq.sum
let dig2 = adders |> Seq.map (fun x -> x.[1]) |> Seq.sum 
let dig3 = adders |> Seq.map (fun x -> x.[2]) |> Seq.sum 
let dig4 = adders |> Seq.map (fun x -> x.[3]) |> Seq.sum 
let dig5 = adders |> Seq.map (fun x -> x.[4]) |> Seq.sum 
let dig6 = adders |> Seq.map (fun x -> x.[5]) |> Seq.sum 
let dig7 = adders |> Seq.map (fun x -> x.[6]) |> Seq.sum 
let dig8 = adders |> Seq.map (fun x -> x.[7]) |> Seq.sum 
let dig9 = adders |> Seq.map (fun x -> x.[8]) |> Seq.sum 
let dig10 = adders |> Seq.map (fun x -> x.[9]) |> Seq.sum 
let dig11 = adders |> Seq.map (fun x -> x.[10]) |> Seq.sum 
let dig12 = adders |> Seq.map (fun x -> x.[11]) |> Seq.sum 

let line1 = adders |> Seq.head |> Array.toList
printfn $"{line1.[4]} {line1.[5]} {line1.[6]}"

let toGamma (i:int64) = if i > 0L then 1L else 0L
let toEpsilon (i:int64) = if i < 0L then 1L else 0L 

let origs = [dig1;dig2;dig3;dig4;dig5;dig6;dig7;dig8;dig9;dig10;dig11;dig12] 
let gammaBits = origs |> List.map toGamma
let epsilonBits = origs |> List.map toEpsilon

let rec toDecimal (acc:int64) (l:list<int64>) =
    if l.IsEmpty then acc
    else toDecimal (acc * 2L + l.Head) (l.Tail) 

let gamma = toDecimal 0L gammaBits
let epsilon = toDecimal 0L epsilonBits

printfn $"File: {dig1} {dig2} {dig3} {dig4} {dig5} {dig6} {dig7} {dig8} {dig9} {dig10} {dig11} {dig12}"

printfn $"Res gammaBits = {gammaBits} gamma={gamma} e={epsilonBits} e={epsilon}"
printfn $"Res = {gamma*epsilon}"

// PART 2

let rec filter (digits: list<array<int64>>) (pos:int) =
    if digits.Length = 1 then digits.Head
    else 
        let filterValue1 = digits |> List.map (fun d -> d.[pos]) |> List.sum 
        let filterValue2 = if filterValue1 > -1L then 1L else -1L 
        let filtered = digits |> List.filter (fun dig -> dig.[pos] = filterValue2)
        filter filtered (pos+1)

let rec filter2 (digits: list<array<int64>>) (pos:int) =
    if digits.Length = 1 then digits.Head
    else 
        let filterValue1 = digits |> List.map (fun d -> d.[pos]) |> List.sum 
        let filterValue2 = if filterValue1 > -1L then -1L else 1L
        let filtered = digits |> List.filter (fun dig -> dig.[pos] = filterValue2)
        filter2 filtered (pos+1)
        
let oxygenValue = filter (adders |> Seq.toList) 0
let co2Value = filter2 (adders |> Seq.toList) 0

let rec toDecimal2 (acc:int64) (l:list<int64>) =
    if l.IsEmpty then acc
    else
        let dig = if l.Head = 1L then 1L else 0L 
        toDecimal2 (acc * 2L + dig) (l.Tail) 

let oxygen = toDecimal2 0L (oxygenValue |> Array.toList)
let co2 = toDecimal2 0L (co2Value |> Array.toList)
printfn $"Filtered: oxygen = {oxygen}" 
printfn $"Filtered: co2 = {co2}"

let lifeSupport = oxygen * co2
printfn $"LifeSupport: {lifeSupport}"
