open System
open System.IO

let lines = File.ReadLines "input.txt" |> Seq.toArray

let digs (line:String) : array<char> =
    let chars = line.ToCharArray () 
    chars 

let toSign (c:char) =
    match c with
    | '1' -> 1L
    | '0' -> -1L
    | c -> failwith $"Not a valid binary digit: {c}"

let adds (line:String) =
    let chars = digs line
    let ints = chars |> Array.map toSign
    ints    

let adders = lines |> Seq.map adds 

let sumColumn (list: seq<array<int64>>) (index: int) : int64 =
    list |> Seq.map (fun x -> x.[index]) |> Seq.sum 

let dig = {0 .. 11} |> Seq.map (adders |> sumColumn) |> Seq.toArray 

let toGamma (i:int64) = if i > 0L then 1L else 0L
let toEpsilon (i:int64) = if i < 0L then 1L else 0L 

let origs = dig |> Seq.toList
let gammaBits = origs |> List.map toGamma
let epsilonBits = origs |> List.map toEpsilon

let rec toDecimal (acc:int64) (l:list<int64>) =
    if l.IsEmpty then acc
    else toDecimal (acc * 2L + l.Head) (l.Tail) 

let gamma = toDecimal 0L gammaBits
let epsilon = toDecimal 0L epsilonBits

printfn $"File: {dig.[0]} {dig.[1]} {dig.[2]} {dig.[3]} {dig.[4]} {dig.[5]} {dig.[6]} {dig.[7]} {dig.[8]} {dig.[9]} {dig.[10]} {dig.[11]}"

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
