open System.IO

let input1 = File.ReadAllLines "input.txt"

let input2 =
    [| "1163751742"
       "1381373672"
       "2136511328"
       "3694931569"
       "7463417111"
       "1319128137"
       "1359912421"
       "3125421639"
       "1293138521"
       "2311944581" |]

let input = input1

type Point = int * int
type Risk = int
type Path = List<Point>
type Points = Set<Point>

let cave =
    input
    |> Array.map
        (fun s ->
            s.ToCharArray()
            |> Array.map (fun c -> (int c) - (int '0')))

let maxX = input.[0].Length - 1
let maxY = input.Length - 1

let risks: Map<Point, Risk> =
    Seq.allPairs { 0 .. maxX } { 0 .. maxY }
    |> Seq.map (fun (x, y) -> ((x, y), cave.[y].[x]))
    |> Map

let points =
    Seq.allPairs { 0 .. maxX } { 0 .. maxY } |> Set

let startPos: Point = 0, 0
let endPos: Point = maxX, maxY

let findNeighborsX (available: Set<Point>) ((x, y): Point) : List<Point> =
    [ (x - 1, y - 1)
      (x, y - 1)
      (x + 1, y - 1)
      (x - 1, y)
      (x + 1, y)
      (x - 1, y + 1)
      (x, y + 1)
      (x + 1, y + 1) ]
    |> Set
    |> Set.intersect available
    |> Set.toList

let riskOfPoint (risks:Map<Point,Risk>) (pos:Point) =
    risks.TryFind pos |> Option.defaultValue 100000

let estimatedRisk (x,y) : Risk = ((maxX - x) + (maxY - y)) * 4

let findNeighbors (available: Set<Point>) ((x, y): Point) : List<Point> =
    let availablePoints : List<Point> = [ x + 1, y
                                          x, y + 1
                                          x - 1, y
                                          x, y - 1 ]
                                        |> List.filter available.Contains
    let cost p = (estimatedRisk p) + (riskOfPoint risks p)
    availablePoints |> List.sortBy cost 


let distToEnd (x, y) : Risk = (maxX - x) + (maxY - y)
let minRisk currPos currRisk : Risk = currRisk + (distToEnd currPos)

let removePrevAvailable (available:Points) (pathBack:Path) =
    if pathBack.Length < 3 then available 
    else
        let (x,y) = pathBack.Tail.Head
        let prevNeighbors = [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
        prevNeighbors |> List.fold (fun acc -> acc.Remove) available

let worstRisk (x,y) = 9*x + 9*y
let isWorseThan (pointRisks:Map<Point,Risk>) (pos:Point) (risk:Risk) =
    let worse = pointRisks.TryFind pos |> Option.map (fun prev -> prev < risk) |> Option.defaultValue false
    // printfn $"isWorseThan {pos} {risk} -> {worse}"
    worse
let rec findNewPath (bestRisk, pointRisks, bestPath) (pathBack: Path) (available: Set<Point>) (risk: Risk) (pos: Point) =
    let available = available.Remove pos
    let available = removePrevAvailable available pathBack
    let pathBack = pos :: pathBack
    let risk = riskOfPoint risks pos + risk
    if isWorseThan pointRisks pos risk then (bestRisk,pointRisks,bestPath) 
    else if minRisk pos risk >= bestRisk then (bestRisk,pointRisks,bestPath)
    else if pos = endPos then
        printfn $"found candidate: risk={risk}"
        risk, pointRisks, pathBack |> List.rev
    else
        // printfn $"Adding pointRisks: {(pos,risk)}"
        let pointRisks = pointRisks.Add (pos,risk)
        let findForPos (best: Risk * Map<Point,Risk> * Path) (pos: Point) : Risk * Map<Point,Risk> * Path =
            findNewPath best pathBack available risk pos

        let neighbors = findNeighbors available pos

        neighbors
        |> List.fold findForPos (bestRisk, pointRisks, bestPath)


// let maxCost = 9 * maxX + 9 * maxY

let maxCost = 749

let bestPath =
    let risk,cache,path = findNewPath (maxCost, Map.empty, [ -1, -1 ]) [] points 0 startPos
    risk,path 

let bestCost =
    fst bestPath - (risks.TryFind(0, 0) |> Option.get)


printfn $"Task 1: {bestCost} {bestPath}"
// printfn $"safest {safest}"
