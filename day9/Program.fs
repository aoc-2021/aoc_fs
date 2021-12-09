open System.IO

let file = File.ReadAllLines "input.txt"
// let file = [| "2199943210"; "3987894921"; "9856789892"; "8767896789"; "9899965678" |]

let toIntArray (s:string) =
    let toInt (c:char) = (int c) - (int '0')
    s.ToCharArray() |> Array.map toInt     
let points = file |> Array.map toIntArray

type Point = int*int 

let allCoordinates : list<Point> =
    let xs = {0..(points.[0].Length - 1)}
    let ys = {0..(points.Length-1)}
    ys |> Seq.map (fun y -> xs |> Seq.map (fun x -> (x,y))) |> Seq.concat |> Seq.toList 

let east = points.[0].Length - 1
let south = points.Length - 1

let findNeighbors ((x,y):Point):list<Point> =
    let all = [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
    all |> List.filter (fun (x,y) -> x > -1 && y > -1 && x <= east && y <= south) 

let valueOf ((x,y):Point) = points.[y].[x]

let isLowerThan (p:Point) (others:list<Point>) =
   let value = valueOf p 
   let values = others |> List.map valueOf
   let lowNeighbors = values |> List.filter (fun o -> o <= value)
   lowNeighbors.Length = 0
   
let isLowPoint (p:Point) =
    let n = findNeighbors p
    isLowerThan p n 
    
let findLows = allCoordinates |> List.filter isLowPoint

let task1 =
    let values = findLows |> List.map valueOf
    let answer = values.Length + (List.sum values)
    printfn $"Task1 {answer}"
    
let basinCoords =
    let isInBasin p = valueOf p < 9 
    allCoordinates |> List.filter isInBasin


let basinHead (pts:list<Point>) : Set<Point>*list<Point> =
    let basin = [|pts.Head|] |> Set
    let rec fillBasin (basin:Set<int*int>) (pts:list<Point>) (origPts:list<Point>) =
        if pts.IsEmpty then basin
        else if basin.Contains pts.Head then fillBasin basin pts.Tail origPts 
        else
            let addToBasin = findNeighbors pts.Head |> List.filter basin.Contains |> List.isEmpty |> not
            if addToBasin then
                let newBasin = basin.Add pts.Head
                fillBasin newBasin origPts origPts
            else
                fillBasin basin pts.Tail origPts 
    let basin = fillBasin basin pts pts
    let remaining = pts |> List.filter (fun p -> not(basin.Contains p))
    basin,remaining 
        
    
let basin1,others = basinHead basinCoords

let rec allBasins (pts:list<Point>) =
    if pts.IsEmpty then []
    else
        let basin,newPts = basinHead pts
        basin :: (allBasins newPts)
        
let basins = allBasins basinCoords

let sizes = basins |> List.map Set.toSeq |> List.map Seq.length 

printfn $"{sizes.Length} {sizes }"

let largest = sizes |> List.sortDescending |> List.take 3
let product = largest |> List.fold (*) 1
printfn $"{product}"