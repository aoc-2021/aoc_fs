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
    risks.TryFind pos |> Option.defaultValue 1000

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

let rec findNewPath (bestRisk, bestPath) (pathBack: Path) (available: Set<Point>) (risk: Risk) (pos: Point) =
    let available = available.Remove pos
    let pathBack = pos :: pathBack

    let risk =
        risks.TryFind pos
        |> Option.defaultValue 100000
        |> ((+) risk)
    // printfn $"risk {risk}"
    if minRisk pos risk >= bestRisk then
        (bestRisk, bestPath)
    else if pos = endPos then
        printfn $"found candidate: risk={risk}"
        risk, pathBack |> List.rev
    else
        let findForPos (best: Risk * Path) (pos: Point) : Risk * Path =
            findNewPath best pathBack available risk pos

        let neighbors = findNeighbors available pos

        neighbors
        |> List.fold findForPos (bestRisk, bestPath)


let maxCost = 9 * maxX + 9 * maxY

let bestPath =
    findNewPath (maxCost, [ -1, -1 ]) [] points 0 startPos

let bestCost =
    fst bestPath - (risks.TryFind(0, 0) |> Option.get)


printfn $"Task 1: {bestCost} {bestPath}"
// printfn $"safest {safest}"
