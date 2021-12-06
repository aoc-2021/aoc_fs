open System.IO

let file = File.ReadAllLines "input.txt"

let fish = file.[0].Split ',' |> Array.map int

printfn $"fish {fish}"

let toCount (x:(int*int[])) =
   let (fish,fishes) = x
   (fish,fishes.Length |> int64)
let fishCount:(int*int64)[] = fish |> Array.groupBy id  |> Array.map toCount

printfn $"{fishCount |> Array.toList}"

let getCountForFish (n:int) (fish:(int*int64)[]) =
   let matches = fish |> Array.filter (fun f -> fst f = n)
   if matches.Length = 1 then snd (matches.[0]) else 0 

let removeFish (fishNo:int) (fish:(int*int64)[]) =
   fish |> Array.filter (fun f -> not((fst f) = fishNo)) 

let addNewFish (fishNo:int) (n:int64) (fish:(int*int64)[]) =
   if getCountForFish fishNo fish > 0L then failwith $"Fish already exists: {fishNo}"
   else (fishNo,n) :: (fish |> Array.toList) |> List.toArray 

let addToFish (fishNo:int) (n:int64) (fish:(int*int64)[]) =
   let current = getCountForFish fishNo fish
   let newCount = current + n
   let withoutFish = removeFish fishNo fish
   addNewFish fishNo newCount withoutFish 

let countDownAll (fish:(int*int64)[]) = fish |> Array.map (fun f -> ((fst f)-1),(snd f))

let fIter (fish:(int*int64)[]) =
   let zeroes = getCountForFish 0 fish
   let fish = removeFish 0 fish
   let fish = addToFish 7 zeroes fish
   let fish = countDownAll fish
   let fish = addNewFish 8 zeroes fish 
   fish
   
let rec fIterN (n:int) (fish:(int*int64)[]) =
   if n = 0 then fish
   else fIterN (n-1) (fIter fish)

let cFish80 = fIterN 80 fishCount 
let cFish256 = fIterN 256 fishCount 

let count fish = fish |> Array.map snd |> Array.sum

printfn $"Part 1 (80 iterations): {count cFish80}"
printfn $"Part 2 (256 iterations): {count cFish256}"