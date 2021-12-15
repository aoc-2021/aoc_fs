open System.IO
open System

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

let lineToInts (line: string) =
    line.ToCharArray()
    |> Array.map (fun c -> (int c) - (int '0'))

let linesToInts (input: string []) = input |> Array.map lineToInts

let incInput (input:int[][]) (n:int) =
    input |> Array.map (fun line -> Array.map ((+) n))

let input = linesToInts input2

let inc  (n:int) (r:int) = if r+n > 9 then (r+n)-9 else r+n  

let multiplyInput (input: int[][]) : int[][] =
    let wider = input
                |> Array.map (fun line ->
                              {0..4}
                              |> Seq.map (fun n -> line |> Array.map (inc n))
                              |> Seq.toArray |> Array.concat)
    let incInput (input: int[][]) (n:int) : int[][] =
        input |> Array.map (fun line -> line |> Array.map (fun c -> inc c n))
    {0..4} |> Seq.map (incInput wider) |> Seq.toArray |> Array.concat


let cave = multiplyInput input 

printfn "11637517422274862853338597396444961841755517295286 <-"
let lineToStr (line:int[]) = line |> String.Concat
cave |> Array.map lineToStr |> Array.map (fun s -> printfn $"{s}")

printfn "67554889357866599146897761125791887223681299833479 <-"

printfn $"{cave.[0].[0]} {cave.[0].[49]}"
printfn $"{cave.[49].[0]} {cave.[49].[49]}"


let maxX = cave.[0].Length - 1
let maxY = cave.Length - 1

type Point = int * int
type Risk = int
type Path = List<Point>
type Points = Set<Point>

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

let riskOfPoint (risks: Map<Point, Risk>) (pos: Point) =
    risks.TryFind pos |> Option.defaultValue 100000

let estimatedRisk (x, y) : Risk = ((maxX - x) + (maxY - y)) * 4

let findNeighbors (available: Set<Point>) ((x, y): Point) : List<Point> =
    let availablePoints: List<Point> =
        [ x + 1, y
          x, y + 1
          x - 1, y
          x, y - 1 ]
        |> List.filter available.Contains

    let cost p =
        (estimatedRisk p) + (riskOfPoint risks p)

    availablePoints |> List.sortBy cost


let distToEnd (x, y) : Risk = (maxX - x) + (maxY - y)
let minRisk currPos currRisk : Risk = currRisk + (distToEnd currPos)

let removePrevAvailable (available: Points) (pathBack: Path) =
    if pathBack.Length < 3 then
        available
    else
        let (x, y) = pathBack.Tail.Head

        let prevNeighbors =
            [ (x - 1, y)
              (x + 1, y)
              (x, y - 1)
              (x, y + 1) ]

        prevNeighbors
        |> List.fold (fun acc -> acc.Remove) available


type Memo(bestRisk: Risk, bestPath: Path, pathRisks: Map<Point, Risk>, forwards: Map<Point, Risk * Path>) =
    member this.BestRisk = bestRisk
    member this.BestPath = bestPath
    member this.PointRisks = pathRisks
    member this.Forwards = forwards

    member this.addPointRisk pos risk =
        Memo(bestRisk, bestPath, pathRisks.Add(pos, risk), forwards)

    member this.withBest (risk: Risk) (path: Path) = Memo(risk, path, pathRisks, forwards)

    member this.HasBetterPath (pos: Point) (risk: Risk) =
        pathRisks.TryFind pos
        |> Option.map ((>=) risk)
        |> Option.defaultValue false

    static member empty startRisk =
        Memo(startRisk, [ (-1, -1) ], Map.empty, Map.empty)

type Track(available: Points, back: Path) =
    member this.Available = available
    member this.last = back.Head
    member this.Path() = back |> List.rev

    member this.addStep(pos: Point) =
        if back.IsEmpty then
            Track(available, [ pos ])
        else
            let newAvailable =
                findNeighbors available back.Head
                |> List.fold (fun (av: Points) p -> av.Remove p) available

            Track(newAvailable, pos :: back)

    static member empty(points: Points) = Track(points, [])

let worstRisk (x, y) = 9 * x + 9 * y

let rec findNewPath (memo: Memo) (track: Track) (risk: Risk) (pos: Point) =
    let track = track.addStep pos
    let risk = riskOfPoint risks pos + risk

    if memo.HasBetterPath pos risk then
        memo
    else if minRisk pos risk >= memo.BestRisk then
        memo
    else if pos = endPos then
        printfn $"found candidate: risk={risk}"
        memo.withBest risk (track.Path())
    else
        // printfn $"Adding pointRisks: {(pos,risk)}"
        let memo = memo.addPointRisk pos risk
        let findForPos (best: Memo) (pos: Point) : Memo = findNewPath best track risk pos

        let neighbors = findNeighbors track.Available pos

        neighbors |> List.fold findForPos memo

// let maxCost = 9 * maxX + 9 * maxY

let maxCost = 749

let bestPath =
    let memo =
        findNewPath (Memo.empty maxCost) (Track.empty points) 0 startPos

    memo.BestRisk, memo.BestPath

let bestCost =
    fst bestPath - (risks.TryFind(0, 0) |> Option.get)


printfn $"Task 1: {bestCost} {bestPath}"
