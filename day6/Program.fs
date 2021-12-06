open System.IO

let file = File.ReadAllLines "input.txt"

let fish = file.[0].Split ',' |> Array.map int

printfn $"fish {fish}"

let iterateFish (fish:int[]) =
   let newFish = fish |> Array.filter ((=) 0) |> Array.map (fun x -> 8)
   let rotated = fish |> Array.map (fun x -> if x = 0 then 6 else x - 1)
   Array.append rotated newFish
   
let x = iterateFish ([2;3;2;0;1] |> List.toArray) |> Array.toList

let rec iterN (n:int) (fish:int[]) =
   if n = 0 then fish
   else iterN (n-1) (iterateFish fish)
   
let iter80 = iterN 80

let fish80 = iter80 fish

printfn $"Fish 80: {fish80.Length}"

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
   
let t = [(0,4L);(4,2L);(7,40L)] |> List.toArray
let t2 = fIter t
printfn $"t {t2 |> Array.toList}"
let r3 = addToFish 7 11L t 
printfn $"Added: {r3 |> Array.toList}"

let rec fIterN (n:int) (fish:(int*int64)[]) =
   printfn $"fish: {fish |> Array.toList}"
   if n = 0 then fish
   else fIterN (n-1) (fIter fish)

let rec cFish80 = fIterN 256 fishCount 

printfn $"{cFish80 |> Array.toList}"

let count fish = cFish80 |> Array.map snd |> Array.sum

printfn $"{count fishCount}"
printfn $"{count cFish80}"
