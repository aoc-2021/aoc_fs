

open System.IO

let file = File.ReadAllLines "input.txt"

// let file = [| "2199943210"; "3987894921"; "9856789892"; "8767896789"; "9899965678" |]

let toInt (c:char) =
    (int c) - (int '0')
    
let points = file |> Array.map (fun (s:string) -> s.ToCharArray () |> Array.map toInt)

let east = points.[0].Length - 1
let south = points.Length - 1

let neighbors ((x,y):int*int) =
    let all = [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
    all |> List.filter (fun (x,y) -> x > -1 && y > -1 && x <= east && y <= south)

let valueOf ((x,y):int*int) = points.[y].[x]

let isLowerThan (p:int*int) (others:list<int*int>) =
   let value = valueOf p 
   let values = others |> List.map valueOf
   let lowNeighbors = values |> List.filter (fun o -> o <= value)
   lowNeighbors.Length = 0
   
let isLowPoint (p:int*int) =
    let n = neighbors p
    isLowerThan p n 
    
let findLows =
    let coords = {0..south}
                 |> Seq.map (fun y -> {0..east} |> Seq.map (fun x -> (x,y)) |> Seq.toList)
                 |> Seq.toList 
                 |> List.concat 
    coords |> List.filter isLowPoint

let task1 =
    let values = findLows |> List.map valueOf
    let answer = values.Length + (List.sum values)
    printfn $"Task1 {answer}"
    
let basinCoords =
    let ys = {0..south} |> Seq.toList
    let xs = {0..east} |> Seq.toList
    let allCoords = ys |> List.map (fun y -> xs |> List.map (fun x -> (x,y))) |> List.concat 
    allCoords |> List.filter (fun p -> (valueOf p) < 9)


let basinHead (pts:list<int*int>) : Set<int*int>*list<int*int> =
    let basin = [|pts.Head|] |> Set
    let rec fillBasin (basin:Set<int*int>) (pts:list<int*int>) (origPts:list<int*int>) =
        if pts.IsEmpty then basin
        else if basin.Contains pts.Head then fillBasin basin pts.Tail origPts 
        else
            let addToBasin = neighbors pts.Head |> List.filter basin.Contains |> List.isEmpty |> not
            if addToBasin then
                let newBasin = basin.Add pts.Head
                fillBasin newBasin origPts origPts
            else
                fillBasin basin pts.Tail origPts 
    let basin = fillBasin basin pts pts
    let remaining = pts |> List.filter (fun p -> not(basin.Contains p))
    basin,remaining 
        
    
let basin1,others = basinHead basinCoords

let rec allBasins (pts:list<int*int>) =
    if pts.IsEmpty then []
    else
        let basin,newPts = basinHead pts
//        printfn $"AllBasins: {basin |> Set.toSeq |> Seq.length} {pts.Length} {newPts.Length}"
        basin :: (allBasins newPts)
        
let basins = allBasins basinCoords

let sizes = basins |> List.map Set.toSeq |> List.map Seq.length 

printfn $"{sizes.Length} {sizes }"

let largest = sizes |> List.sortDescending |> List.take 3
let product = largest |> List.fold (*) 1
printfn $"{product}"
    
    