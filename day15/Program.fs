open System.IO

let file = File.ReadAllLines "input.txt"

let lineToInts (line: string) =
    line.ToCharArray()
    |> Array.map (fun c -> (int c) - (int '0'))

let linesToInts (input: string []) = input |> Array.map lineToInts

let input = linesToInts file

let inc (n: int) (r: int) =
    if r + n > 9 then (r + n) - 9 else r + n

let multiplyInput (input: int [] []) : int [] [] =
    let wider =
        input
        |> Array.map
            (fun line ->
                { 0 .. 4 }
                |> Seq.map (fun n -> line |> Array.map (inc n))
                |> Seq.toArray
                |> Array.concat)

    let incInput (input: int [] []) (n: int) : int [] [] =
        input
        |> Array.map (fun line -> line |> Array.map (fun c -> inc c n))

    { 0 .. 4 }
    |> Seq.map (incInput wider)
    |> Seq.toArray
    |> Array.concat


let cave = multiplyInput input

let maxX = cave.[0].Length - 1
let maxY = cave.Length - 1

// input done


type Point = int * int
type Points = List<Point>
type Risk = int
type Risks = Map<Point, Risk * Risk>
type Cave = int [] []

let startPoint = 0, 0
let endPoint = maxX, maxY

type State(risks: Risks, changed: bool) =
    member this.Changed = changed
    member this.RiskMap = risks

    member this.Risks(point: Point) =
        match risks.TryFind point with
        | None ->
            100000, 1000000
        | Some risks -> risks

    member this.RegisterPathRisk (point: Point) (pathRisk: Risk) : State =
        let risks = this.Risks point
        let risk, oldRisk = risks

        if oldRisk <= pathRisk then
            this
        else
            let risks =
                this.RiskMap.Add(point, (risk, pathRisk))

            let state = State(risks, true)
            state

    member this.Untainted = State(risks, false)

    override this.ToString() =
        $"State({risks},{changed}) ${(22, 20)}={risks.TryFind(22, 20)}"

let points =
    Seq.allPairs { 0 .. maxX } { 0 .. maxY }
    |> Seq.toList

let toInitState (points: Points) (cave: Cave) : State =
    let pointMap =
        points
        |> List.map (fun (x, y) -> ((x, y), (cave.[y].[x], 1000000)))
        |> Map

    let pointMap = pointMap.Add((0, 0), (cave.[0].[0], 0))
    State(pointMap, true)

let findNeighbors ((x, y): Point) : Points =
    let isValid (x, y) =
        x >= 0 && x <= maxX && y >= 0 && y <= maxY

    [ (x - 1, y)
      (x + 1, y)
      (x, y - 1)
      (x, y + 1) ]
    |> List.filter isValid

let pointsAndNeighbors =
    points |> List.map (fun p -> p, findNeighbors p)

let update (state: State) (point, neighbors) =
    let risk, _ = state.Risks point

    let candRisk =
        neighbors
        |> List.map state.Risks
        |> List.map snd
        |> List.map ((+) risk)
        |> List.min

    let state = state.RegisterPathRisk point candRisk
    state

let initState = toInitState points cave

let rec search (state: State) pointsAndNeighbors =
    let state = state.Untainted

    let state =
        pointsAndNeighbors |> List.fold update state

    if state.Changed then
        search state pointsAndNeighbors
    else
        state

let endState = search initState pointsAndNeighbors
let endValue = endState.Risks endPoint |> snd

printfn $"{endValue}"
