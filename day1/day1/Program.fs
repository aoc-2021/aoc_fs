open System.IO

let depths = File.ReadAllLines "input.txt" |> Array.map int 

let slides =
   let indexes = seq { 2 .. (depths.Length - 1) }
   let slideSum n = depths.[n] + depths.[n-1] + depths.[n-2]
   indexes |> Seq.map slideSum |> Seq.toArray 
   
let diffs (depths:array<int>) =
    let deltas = seq { 1 .. (depths.Length-1) }
    let delta (i:int): int = depths.[i] - depths.[i-1]
    deltas |> Seq.map delta |> Seq.toArray

let pos = (<) 0  
let incs = diffs depths |> Seq.filter pos |> Seq.length
let slideIncs = diffs slides |> Seq.filter pos |> Seq.length 

printfn $"Solution 1: %A{incs}"
printfn $"Solution 2: %A{slideIncs}"
