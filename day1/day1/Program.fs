

// For more information see https://aka.ms/fsharp-console-apps

open System
open System.IO

printfn "Hello from F#"

let readFile(filePath:String) = seq {
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () 
}

let toInt (x:String) : int = x |> int 
let depths = readFile("input.txt") |> Seq.map toInt |> Seq.toArray
let slides = seq { 2 .. (depths.Length - 1) }

let slideDepth n = depths.[n] + depths.[n-1] + depths.[n-2]

let slideDepths = slides |> Seq.map slideDepth |> Seq.toArray 

let deltas = seq { 1 .. (depths.Length-1) }
let slideDeltas = seq { 1 .. (slideDepths.Length-1) }

let delta (i:int) : int = depths.[i] - depths.[i-1]
let slideDelta (i:int) : int = slideDepths.[i] - slideDepths.[i-1]
let diffs = deltas |> Seq.map delta |> Seq.toArray
let slideDiffs = slideDeltas |> Seq.map slideDelta |> Seq.toArray 

let pos x = x > 0
let incs = diffs |> Seq.filter pos |> Seq.toArray |> Array.length
let slideIncs = slideDiffs |> Seq.filter pos |> Seq.toArray |> Array.length 

printfn $"Solution 1: %A{incs}"
printfn $"Solution 2: %A{slideIncs}"
