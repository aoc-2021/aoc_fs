open System
open System.IO

let readFile(filePath:String) = seq {
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () 
}

let depths = readFile("input.txt") |> Seq.map int |> Seq.toArray

let slides =
   let indexes = seq { 2 .. (depths.Length - 1) }
   let slideSum n = depths.[n] + depths.[n-1] + depths.[n-2]
   indexes |> Seq.map slideSum |> Seq.toArray 
   
let deltas = seq { 1 .. (depths.Length-1) }

let slideDeltas = seq { 1 .. (slides.Length-1) }

let delta  (depths:array<int>) (i:int): int = depths.[i] - depths.[i-1]
let diffs = deltas |> Seq.map (delta depths) |> Seq.toArray
let slideDiffs = slideDeltas |> Seq.map (delta slides) |> Seq.toArray 

let pos x = x > 0
let incs = diffs |> Seq.filter pos |> Seq.toArray |> Array.length
let slideIncs = slideDiffs |> Seq.filter pos |> Seq.toArray |> Array.length 

printfn $"Solution 1: %A{incs}"
printfn $"Solution 2: %A{slideIncs}"
