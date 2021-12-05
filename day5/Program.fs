

open System.IO
open System.Text.RegularExpressions

let lines = File.ReadAllLines "input.txt"

let pipes = lines |> Array.map (fun s -> Regex.Replace (s," -> ", ","))
            |> Array.map (fun s -> s.Split ',')
            |> Array.map (fun s -> s |> Array.map int)

// printfn $"{pipesX.[3].[3]}"

// let isGrid (line:int[]) = line.[0] = line.[2] || line.[1] = line.[3]

// let pipes = pipesX |> Array.filter isGrid 

let stepTo v1 v2 = if v1 < v2 then v1+1 else v1-1

let rec explode (pipe:array<int>) : list<(int * int)> =
    let x1 = pipe.[0]
    let y1 = pipe.[1]
    let x2 = pipe.[2]
    let y2 = pipe.[3]
    let point = (x1,y1)
    // printfn $"point:{point}"
    if x1 = x2 && y1 = y2 then
        point::[]
    else
        let nextArray =
            if x1 = x2 then [x1;(stepTo y1 y2);x2;y2] |> Seq.toArray
            else [(stepTo x1 x2);y1;x2;y2] |> Seq.toArray 
        point::(explode nextArray)
                
let x = explode pipes.[0]
printfn "hello"
printfn $"{x.[0]} {x.[1]}"

let pointsR = pipes |> Array.map explode |> Array.toList 

printfn $"{pointsR}"

let rec flattenList rlist =
    match rlist with
    | [] -> []
    | []::tail -> flattenList tail 
    | (a::at)::tail -> a :: (flattenList (at::tail))

let points = List.concat pointsR
printfn $"{points}"

let grouped = points |> List.groupBy id

let toCounts (x:((int*int)*list<(int*int)>)) =
    let (a,b) = x
    b.Length
    
let counts = grouped |> List.map toCounts 
let crossed = counts |> Seq.filter (fun x -> x > 1) |> Seq.toList 
let numCrossed = crossed.Length

printfn $"{numCrossed}"
    