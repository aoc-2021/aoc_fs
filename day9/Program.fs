open System.IO

let file = File.ReadAllLines "input.txt"
// let file = [| "2199943210"; "3987894921"; "9856789892"; "8767896789"; "9899965678" |]
  
let points =
    let toIntArray (s:string) =
        let toInt (c:char) = (int c) - (int '0')
        s.ToCharArray() |> Array.map toInt    
    file |> Array.map toIntArray

type Point = int*int
type Basin = Set<Point> 

let valueOf ((x,y):Point) = points.[y].[x]

let east = points.[0].Length - 1
let south = points.Length - 1

let allPoints : list<Point> =
    let xs = {0..east}
    let ys = {0..south} 
    ys |> Seq.map (fun y -> xs |> Seq.map (fun x -> (x,y))) |> Seq.concat |> Seq.toList 

let basinPoints : list<Point> =
    let inBasin p = valueOf p < 9 
    allPoints |> List.filter inBasin 

let findNeighbors ((x,y):Point):list<Point> =
    let all = [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
    all |> List.filter (fun (x,y) -> x > -1 && y > -1 && x <= east && y <= south) 

// Task 1 

let isLowPoint (p:Point) =
   let value = valueOf p 
   let values = findNeighbors p |> List.map valueOf
   values |> List.exists (fun o -> o <= value) |> not 
   
let findLows = allPoints |> List.filter isLowPoint

let task1 =
    let values = findLows |> List.map valueOf
    let answer = values.Length + (List.sum values)
    printfn $"Task1 {answer}"

// Task 2 take 2

let rec assignPoints (basins:List<Basin>) (unassigned:list<Point>) : List<Basin> =
    if unassigned.IsEmpty then basins
    else
        let point = unassigned.Head
        let neighbors = findNeighbors point 
        let hasPoint (basin:Basin) = neighbors |> List.exists basin.Contains 
        let connected,rest = basins |> List.partition hasPoint
        let newBasin = ((Set.singleton point) :: connected) |> Set.unionMany
        let basins = newBasin::rest
        assignPoints basins unassigned.Tail  
let task2 =
    let initialBasin = [Set.singleton basinPoints.Head]
    let unassigned = basinPoints.Tail 
    let basins = assignPoints initialBasin unassigned
    let sizes = basins |> List.map Set.count
    let answer = sizes |> List.sortDescending |> List.take 3 |> List.fold (*) 1
    printfn $"Task 2b: {answer}" 