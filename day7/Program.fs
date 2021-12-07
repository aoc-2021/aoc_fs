open System.IO

let file = File.ReadAllLines "input.txt"

let positions = file.[0].Split ',' |> Array.map int |> Array.sort 
let min = positions.[0]
let max = positions.[positions.Length-1]
printfn $"positions {min} {max}"

let firstStep = max-min
let valueOf (target:int) (crabs:int[]) =
    // let crabValue target crab = abs(crab - target)
    let crabValue target crab =
        let steps = abs(crab - target)
        let rec cost (acc:int64) (step:int64) (rem:int64) =
            if rem = 0L then acc
            else (cost (acc+step) (step+1L) (rem-1L))
        cost 0L 1L (steps |> int64)
    crabs |> Array.map (crabValue target) |> Array.sum  
    
let pos = {min .. max} |> Seq.map (fun i -> i, valueOf i positions)
          |> Seq.sortBy snd
          |> Seq.toArray
let answer = fst pos.[0]
printfn $"Task 1 : {pos.[0]}"
            