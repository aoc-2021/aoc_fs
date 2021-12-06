open System.IO

let file = File.ReadAllLines "input.txt"

let fish = file.[0].Split ',' |> Array.map int

printfn $"fish {fish}"

let toCount (x:(int*int[])) =
   let (fish,fishes) = x
   (fish,fishes.Length |> int64)
let fishCount:(int*int64)[] = fish |> Array.groupBy id  |> Array.map toCount

printfn $"{fishCount |> Array.toList}"

let toMap (fish:(int*int64)[]) :Map<int,int64> =
   fish |> Array.toSeq |> Map 

let getCountForFish (n:int) (fish:Map<int,int64>) =
   match fish.TryFind n with
      | None -> 0L
      | Some(fishCount) -> fishCount 

let removeFish (fishNo:int) (fish:Map<int,int64>) =
   fish.Remove fishNo |> Map.toArray 

let addNewFish (fishNo:int) (n:int64) (fish:Map<int,int64>) =
   if getCountForFish fishNo fish > 0L then failwith $"Fish already exists: {fishNo}"
   fish.Add (fishNo,n) |> Map.toArray 

let addToFish (fishNo:int) (n:int64) (fish:(int*int64)[]) =
   let current = getCountForFish fishNo (toMap fish)
   let newCount = current + n
   let withoutFish = removeFish fishNo (toMap fish)
   addNewFish fishNo newCount (toMap withoutFish) 

let countDownAll (fish:Map<int,int64>) =
   fish |> Map.toArray |> Array.map (fun f -> ((fst f)-1),(snd f))

let fIter (fish:(int*int64)[]) =
   let zeroes = getCountForFish 0 (toMap fish)
   let fish = removeFish 0 (toMap fish)
   let fish = addToFish 7 zeroes fish
   let fish = countDownAll (toMap fish)
   let fish = addNewFish 8 zeroes (toMap fish) 
   fish
   
let rec fIterN (n:int) (fish:(int*int64)[]) =
   if n = 0 then fish
   else fIterN (n-1) (fIter fish)

let cFish80 = fIterN 80 fishCount 
let cFish256 = fIterN 256 fishCount 

let count fish = fish |> Array.map snd |> Array.sum

printfn $"Part 1 (80 iterations): {count cFish80}"
printfn $"Part 2 (256 iterations): {count cFish256}"