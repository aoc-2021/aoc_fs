type Cost = uint64
type Pos = int * int

type Amp =
    | A
    | B
    | C
    | D
    member this.AsChar =
        match this with
        | A -> 'A'
        | B -> 'B'
        | C -> 'C'
        | D -> 'D'

    member this.Cost: Cost =
        match this with
        | A -> 1UL
        | B -> 10UL
        | C -> 100UL
        | D -> 1000UL

    member this.Column =
        match this with
        | A -> 2
        | B -> 4
        | C -> 6
        | D -> 8

    member this.HexValue: uint64 =
        match this with
        | A -> 1UL
        | B -> 2UL
        | C -> 4UL
        | D -> 8UL

    member this.HexComplement: uint64 =
        match this with
        | A -> 0xFUL ^^^ 1UL
        | B -> 0xFUL ^^^ 2UL
        | C -> 0xFUL ^^^ 4UL
        | D -> 0xFUL ^^^ 8UL

    member this.Mask: uint64 = 0xFUL
    member this.ComplementMask: uint64 = 0xFUL ^^^ 0UL

let testInput = [ (B, A); (C, D); (B, C); (D, A) ]
let prodInput = [ (D, D); (A, C); (C, B); (A, B) ]

let allCoordinates: Set<Pos> =
    let hallway: List<Pos> =
        [ 0 .. 10 ] |> List.map (fun x -> (x, 0))

    let rooms =
        List.allPairs [ 2; 4; 6; 8 ] [
            1
            2
            3
            4
        ]

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
    member this.Moves() = moves |> List.rev
    member this.Depth = depth 

    member this.MemoKey: string =
        allCoordinates
        |> Set.toList
        |> List.sort
        |> List.map amps.TryFind
        |> List.map (fun o -> o |> Option.map (fun amp -> amp.AsChar))
        |> List.map (fun o -> o |> Option.defaultValue '.')
        |> List.toArray
        |> System.String

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
            |> Seq.filter (fun (_, y) -> y <> 0)
            |> Seq.toList

        let movable ((x, y): Pos) = amps.ContainsKey(x, y - 1) |> not // no one outside

        let inPlace ((x, y): Pos) =
            let amp = amps.TryFind((x, y)) |> Option.get

            if columnOf amp <> x then
                false // wrong room
            else
                let b1 =
                    (amps.TryFind(x, y + 1) |> Option.defaultValue amp) = amp // not a wrong amp below

                let b2 =
                    (amps.TryFind(x, y + 2) |> Option.defaultValue amp) = amp // below that again

                let b3 =
                    (amps.TryFind(x, y + 3) |> Option.defaultValue amp) = amp // basement or below

                b1 && b2 && b3

        roomAmps
        |> List.filter movable
        |> List.filter (inPlace >> not) //  (fun pos -> inPlace pos |> not)

    member this.HomeFillingPos(parked: Pos) : Option<Pos> =
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
                let homePos = Some(find (tx, depth))
                if (amps.ContainsKey (homePos |> Option.get)) then
                    let msg = $"Chose an occupied home pos: {parked} {homePos}" +
                              $"\n burrow = {this.ToBurrowMap ()}\n"
                    failwith msg 
                homePos 


    member this.PathToHome (pos: Pos) ((homeX, homeY): Pos) : bool =
        let rec walkHome (pos: Pos) : bool =
            if pos = (homeX, homeY) then
                true
            else
                let nextPos =
                    match pos with
                    | x, y when x < homeX -> (x + 1, y)
                    | x, y when x > homeX -> (x - 1, y)
                    | x, y -> (x, y + 1)

                match amps.TryFind nextPos with
                | Some _ -> false
                | None -> walkHome nextPos

        walkHome pos

    member this.AvailableParkingSpots((x, _): Pos) : list<Pos> =
        let legalParking (x, _) = x <> 2 && x <> 4 && x <> 6 && x <> 8

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

        let addedCost = ((dx + dy) |> uint64) * amp.Cost
        let cost = cost + addedCost

        let amps =
            amps.Remove((x, y)).Add((destX, destY), amp)

        let newBurrow = Burrow(amps, cost, ((x, y), (destX, destY), addedCost) :: moves, depth)
        if newBurrow.Amps.Count % 2 = 1 then
            let from = this.ToBurrowMap ()
            let into = newBurrow.ToBurrowMap() 
            let msg = $"Ate an amphipode: {(x,y)} -> {(destX,destY)}" +
                      $"when moving from \n{from}\n" +
                      $"to \n{into}\n"
            failwith msg 
        newBurrow 
          
            

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

    member this.ToBurrowMap () =
        let ampToChar (amp: Amp) = amp.AsChar

        let toChar (pos: Pos) : char =
            if (fst pos) = 12 then
              '\n'
            elif allCoordinates.Contains pos then
                amps.TryFind pos
                |> Option.map ampToChar
                |> Option.defaultValue '.'
            else
                '???'

        Array.allPairs [| -1 .. 5 |] [| -1 .. 12 |]
        |> Array.map (fun (y, x) -> (x, y))
        |> Array.map toChar
        |> System.String

let printBurrow (burrow: Burrow) =
    let map = burrow.ToBurrowMap ()
    printfn $"Burrow [{burrow.Depth}]:\n{map}"

type Memo(memo: Map<string, Cost>, best: Option<Burrow>) =
    member this.Map = memo
    member this.Best = best

    member this.Roof =
        best
        |> Option.map (fun burrow -> burrow.Cost)
        |> Option.defaultValue 1_000_000_000UL

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
    |> (fun amps -> Burrow(amps, 0UL, [], 2))

let toPart2Burrow (input: List<Amp * Amp>) =
    let amps: List<Pos * Amp> =
        input
        |> toAmpPositions
        |> Map.toList
        |> List.map
            (fun ((x, y), amp) ->
                let y = if y = 2 then 4 else y
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
    |> (fun amps -> Burrow(amps, 0UL, [], 4))

let findNexts (burrow: Burrow) : List<Burrow> =
    let homebounds: List<Pos * Pos> =
        burrow.ParkedAmps()
        |> List.map (fun pos -> pos, burrow.HomeFillingPos pos)
        |> List.filter (fun (_, target) -> target <> None)
        |> List.map (fun (pos, target) -> (pos, target |> Option.get))
        |> List.filter (fun (pos, target) -> burrow.PathToHome pos target)
        
    if homebounds <> [] then
        let burrow =
            let (from,into) = homebounds.Head
            burrow.Move from into 
        [ burrow ]
    else
        let roomies = burrow.MovableRoomAmps()

        if roomies.IsEmpty then
            printfn "No more moves for:"
            printBurrow burrow

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

let prodBurrow = toPart2Burrow prodInput
let testBurrow = toPart2Burrow testInput

let initBurrow = testBurrow

printfn $"memo: {testBurrow.MemoKey}"
let nexts = findNexts initBurrow

printfn $"nexts = {nexts}"

let memo = find Memo.empty initBurrow
let final = memo.Best
let answerString : string =
                   final
                   |> Option.map (fun burrow -> $"Cost: {burrow.Cost}")
                   |> Option.defaultValue "No solution found"
printfn $"Answer: {answerString}"

printfn $"moves: "
if final.IsSome then 
    let final = final |> Option.get
    final.Moves()
    |> List.map (fun (start, finish, cost) -> printfn $"{start}->{finish} [{cost}]")
    ()

printfn ""

printfn "test input, part 2: "
toPart2Burrow testInput |> printBurrow
