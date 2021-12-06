open System.IO

let file = File.ReadAllLines "input.txt"

let toCount ((fish,fishes):int*int[]) = (fish,fishes.Length |> int64)
let fish = file.[0].Split ',' |> Array.map int
           |> Array.groupBy id
           |> Array.map toCount
           |> Map 

let getCountForFish (fishNo:int) (fish:Map<int,int64>) =
   fish.TryFind fishNo |> Option.defaultValue 0L

let addToFish (fishNo:int) (n:int64) (fish:Map<int,int64>) =
   let current = fish |> getCountForFish fishNo
   fish.Add (fishNo, current + n)

let countDownAll (fish:Map<int,int64>) : Map<int,int64> =
   let decKey (fish,count) = (fish-1,count)
   fish |> Map.toSeq |> Seq.map decKey |> Map

let fIter (fish:Map<int,int64>) =
   let zeroes = getCountForFish 0 fish
   let fish = fish.Remove 0
   let fish = addToFish 7 zeroes fish
   let fish = countDownAll fish
   let fish = fish.Add (8,zeroes)
   fish 
   
let rec fIterN (n:int) (fish:Map<int,int64>) =
   if n = 0 then fish
   else fIterN (n-1) (fIter fish)

let cFish80 = fIterN 80 fish
let cFish256 = fIterN 256 fish

let count fish = fish |> Map.values |> Seq.sum 

printfn $"Part 1 (80 iterations): {count cFish80}"
printfn $"Part 2 (256 iterations): {count cFish256}"