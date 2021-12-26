type Amp =
    | A
    | B
    | C
    | D

type Cost = int64

type Pos = int * int

let moveCost (amp: Amp) =
    match amp with
    | A -> 1L
    | B -> 10L
    | C -> 100L
    | D -> 1000L

let testInput = [ (B, A); (C, D); (B, C); (D, A) ]
let prodInput = [ (D, D); (A, C); (C, B); (A, B) ]

let allCoordinates: Set<Pos> =
    let hallway: List<Pos> =
        [ 0 .. 11 ] |> List.map (fun x -> (x, 0))

    let rooms = List.allPairs [ 2; 4; 6; 8 ] [ 1; 2 ]
    List.concat [ hallway; rooms ] |> Set

let toAmpPositions ([ (a1, a2); (b1, b2); (c1, c2); (d1, d2) ]: list<Amp * Amp>) : Map<Pos, Amp> =
    let a1 = ((2, 1), a1)
    let a2 = ((2, 2), a2)
    let b1 = ((4, 1), b1)
    let b2 = ((4, 2), b2)
    let c1 = ((6, 1), c1)
    let c2 = ((6, 2), c2)
    let d1 = ((8, 1), d1)
    let d2 = ((8, 2), d2)
    [ a1; a2; b1; b2; c1; c2; d1; d2 ] |> Map

let columnOf (amp: Amp) =
    match amp with
    | A -> 2
    | B -> 4
    | C -> 6
    | D -> 8

let walkCost ((x1, y1): Pos) ((x2, y2): Pos) = abs (x1 - x2) + abs (y2 - y1)

type Burrow(amps: Map<Pos, Amp>, cost: Cost, moves: List<Pos * Pos * Cost>, depth: int) =
    member this.Amps = amps
    member this.Cost = cost
    member this.Moves = moves

    member this.MemoKey: string =
        allCoordinates
        |> Set.toList
        |> List.sort
        |> List.map amps.TryFind
        |> List.map (fun o -> o |> Option.map (fun c -> c.ToString()))
        |> List.map (fun o -> o |> Option.defaultValue ".")
        |> String.concat ""

    override this.ToString() : string = $"Burrow({this.MemoKey} {this.Cost})"

    member this.ParkedAmps() =
        amps
        |> Map.keys
        |> Seq.filter (fun (_, y) -> y = 0)
        |> Seq.toList

    member this.MovableRoomAmps() =
        let roomAmps =
            amps
            |> Map.keys
            |> Seq.filter (fun (x, y) -> y <> 0)
            |> Seq.toList

        let movable ((x, y): Pos) = amps.ContainsKey(x, y - 1) |> not // no one outside

        let inPlace ((x, y): Pos) =
            let amp = amps.TryFind((x, y)) |> Option.get

            if columnOf amp <> x then
                false // wrong room
            else
                (amps.TryFind(x, y + 1) |> Option.defaultValue amp) = amp // not a wrong amp below

        roomAmps
        |> List.filter movable
        |> List.filter (fun pos -> inPlace pos |> not)

    member this.AvailableHome(parked: Pos) : Option<Pos> =
        match amps.TryFind parked with
        | None -> None
        | Some (amp) ->
            let tx = columnOf amp

            let available =
                amps
                |> Map.toList
                |> List.filter (fun ((x, _), _) -> x = tx)
                |> List.filter (fun ((x, _), amp) -> x <> columnOf amp)
                |> List.isEmpty

            if not available then
                None
            else
                let rec find (x, y) =
                    if amps.ContainsKey(x, y) then
                        find (x, y - 1)
                    else
                        (x, y)

                Some(find (tx, depth))


    member this.PathToHome (pos: Pos) ((homeX, homeY): Pos) : bool =
        let rec walkHome (pos: Pos) : bool =
            if pos = (homeX, homeY) then
                true
            else
                let nextPos =
                    match pos with
                    | (x, y) when x < homeX -> (x + 1, y)
                    | (x, y) when x > homeX -> (x - 1, y)
                    | (x, y) -> (x, y + 1)

                match amps.TryFind nextPos with
                | Some _ -> false
                | None -> walkHome nextPos

        walkHome pos

    member this.AvailableParkingSpots((x, _): Pos) : list<Pos> =
        let legalParking (x, y) = x <> 2 && x <> 4 && x <> 6 && x <> 8

        let rec findLeft (x, y) =
            if x < 0 || amps.ContainsKey(x, y) then
                []
            else if legalParking (x, y) then
                (x, y) :: (findLeft (x - 1, y))
            else
                findLeft (x - 1, y)

        let rec findRight (x, y) =
            if x > 10 || amps.ContainsKey(x, y) then
                []
            else if legalParking (x, y) then
                (x, y) :: (findRight (x + 1, y))
            else
                findRight (x + 1, y)

        let lefts = findLeft (x - 1, 0)
        let rights = findRight (x + 1, 0)
        List.concat [ lefts; rights ]

    member this.Move ((x, y): Pos) ((destX, destY): Pos) : Burrow =
        let amp = amps.TryFind(x, y) |> Option.get // assume there's someone to move
        let dx = abs (destX - x)
        let dy = abs (destY - y)

        let addedCost = ((dx + dy) |> int64) * (moveCost amp)
        let cost = cost + addedCost

        let amps =
            amps.Remove((x, y)).Add((destX, destY), amp)

        Burrow(amps, cost, ((x, y), (destX, destY), addedCost) :: moves,2)

    member this.IsWin() =
        if amps.Count = 8 then
            amps |> Map.toList |> List.sort = [ ((2, 1), A)
                                                ((2, 2), A)
                                                ((4, 1), B)
                                                ((4, 2), B)
                                                ((6, 1), C)
                                                ((6, 2), C)
                                                ((8, 1), D)
                                                ((8, 2), D) ]
        else
            amps |> Map.toList |> List.sort = [ ((2, 1), A)
                                                ((2, 2), A)
                                                ((2, 3), A)
                                                ((2, 4), A)
                                                ((4, 1), B)
                                                ((4, 2), B)
                                                ((4, 3), B)
                                                ((4, 4), B)
                                                ((6, 1), C)
                                                ((6, 2), C)
                                                ((6, 3), C)
                                                ((6, 4), C)
                                                ((8, 1), D)
                                                ((8, 2), D)
                                                ((8, 3), D)
                                                ((8, 4), D) ]

type Memo(memo: Map<string, Cost>, best: Option<Burrow>) =
    member this.Map = memo
    member this.Best = best

    member this.Roof =
        best
        |> Option.map (fun burrow -> burrow.Cost)
        |> Option.defaultValue 1_000_000_000

    override this.ToString() = $"Memo ({memo.Count} best:{this.Roof})"

    member this.Register(burrow: Burrow) : Memo * bool =
        match memo.TryFind burrow.MemoKey with
        | Some (prevCost) when prevCost <= burrow.Cost -> this, false
        | _ ->
            let memo =
                Memo(memo.Add(burrow.MemoKey, burrow.Cost), best)

            let newBest =
                if burrow.IsWin() && burrow.Cost < memo.Roof then
                    Some(burrow)
                else
                    best

            Memo(memo.Map, newBest), true

    static member empty = Memo(Map.empty, None)

let toPart1Burrow (input: List<Amp * Amp>) =
    input
    |> toAmpPositions
    |> (fun amps -> Burrow(amps, 0L, [],2))

let toPart2Burrow (input: List<Amp * Amp>) =
    let amps: List<Pos * Amp> =
        input
        |> toAmpPositions
        |> Map.toList
        |> List.map
            (fun ((x, y), amp) ->
                let y = if y = 1 then 1 else 4
                ((x, y), amp))

    let more =
        [ ((2, 2), D)
          ((2, 3), D)
          ((4, 2), C)
          ((4, 3), B)
          ((6, 2), B)
          ((6, 3), A)
          ((8, 2), A)
          ((8, 3), C) ]

    List.concat [ amps; more ]
    |> Map 
    |> (fun amps -> Burrow(amps, 0L, [],4))

let findNexts (burrow: Burrow) : List<Burrow> =
    let homebounds: List<Pos * Pos> =
        burrow.ParkedAmps()
        |> List.map (fun pos -> pos, burrow.AvailableHome pos)
        |> List.filter (fun (_, target) -> target <> None)
        |> List.map (fun (pos, target) -> (pos, target |> Option.get))
        |> List.filter (fun (pos, target) -> burrow.PathToHome pos target)


    if homebounds <> [] then
        let burrow =
            homebounds
            |> List.fold (fun (burrow: Burrow) (parked, home) -> burrow.Move parked home) burrow

        [ burrow ]
    else
        let roomies = burrow.MovableRoomAmps()

        let roomieMoves =
            roomies
            |> List.map
                (fun roomie ->
                    burrow.AvailableParkingSpots roomie
                    |> List.map (fun spot -> (roomie, spot)))
            |> List.concat


        let burrows =
            roomieMoves
            |> List.map (fun (startPos, endPos) -> burrow.Move startPos endPos)
            |> List.sortBy (fun burrow -> burrow.Cost)

        burrows

let rec find (memo: Memo) (burrow: Burrow) : Memo =
    if memo.Map.Count % 1000 = 0 then
        printfn $"{memo} {burrow.Cost}"

    let tooExpensive = burrow.Cost > memo.Roof
    let memo, better = memo.Register burrow

    if not better || tooExpensive then
        memo
    else
        findNexts burrow |> List.fold find memo

let prodBurrow = toPart1Burrow prodInput
let testBurrow = toPart1Burrow testInput

printfn $"memo: {testBurrow.MemoKey}"
let nexts = findNexts prodBurrow

printfn $"nexts = {nexts}"

let memo = find Memo.empty prodBurrow
let final = memo.Best |> Option.get

printfn $"moves: "
final.Moves |> List.map (fun p -> printfn $"{p}")
