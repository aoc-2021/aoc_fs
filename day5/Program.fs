open System.IO
open System.Text.RegularExpressions

let lines = File.ReadAllLines "input.txt"

let pipes = lines |> Array.map (fun s -> Regex.Replace (s," -> ", ","))
            |> Array.map (fun s -> s.Split ',')
            |> Array.map (fun s -> s |> Array.map int)

let stepTo v1 v2 =
    if v1 < v2 then v1+1
    else if v1 > v2 then v1-1 else v1 

let rec explode (pipe:array<int>) : list<(int * int)> =
    let x1 = pipe.[0]
    let y1 = pipe.[1]
    let x2 = pipe.[2]
    let y2 = pipe.[3]
    let point = (x1,y1)
    if x1 = x2 && y1 = y2 then
        point::[]
    else
        let nextArray = [(stepTo x1 x2);(stepTo y1 y2);x2;y2] |> Seq.toArray
        point::(explode nextArray)
                
let pointsR = pipes |> Array.map explode |> Array.toList 

let points = List.concat pointsR

let grouped = points |> List.groupBy id

let toCounts ((_,b):_*list<int*int>) = b.Length
    
let counts = grouped |> List.map toCounts 
let crossed = counts |> Seq.filter (fun x -> x > 1) |> Seq.toList 
let numCrossed = crossed.Length

printfn $"{numCrossed}"