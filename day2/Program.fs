open System
open System.IO

let splitLine (s:string) : array<string> = s.Split ' '
let toDelta (c:array<String>) =
    match c.[0] with
    | "forward" -> ((c.[1] |> int),0)
    | "up" -> (0,-(c.[1] |> int))
    | "down" -> (0, (c.[1]|> int))
    | cmd -> failwith $"Unknown command: {cmd}"

let commands = File.ReadAllLines "input.txt" |> Array.map splitLine
let deltas = commands |> Array.map toDelta 
let moveAndAims = deltas |> Seq.toList 

let depths = deltas |> Array.map snd |> Array.sum 
let forwards = deltas |> Array.map fst |> Array.sum 

printfn $"Task 1: {depths*forwards}"

let adjustPos ((currFor,currDepth,currAim)) (forward,aimDelta) =
    let newForward = currFor + forward
    let newDepth = currDepth + (forward*currAim)
    let newAim = currAim + aimDelta
    (newForward,newDepth,newAim)
    
let finalPos = moveAndAims |> List.fold adjustPos (0,0,0)

let toAnswer (a,b,_) = (a |> int64) * (b |> int64)
let answer = toAnswer finalPos

printfn $"Task 2: {finalPos} {answer}"

