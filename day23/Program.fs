open System.IO

let file = File.ReadAllLines "input.txt"

type Amp =
    | A
    | B
    | C
    | D

let stepCost (amp: Amp) : int64 =
    match amp with
    | A -> 1L
    | B -> 10L
    | C -> 20L
    | D -> 30L

type Pos = int * int
let nowhere: Pos = -1, -1

let hallway =
    { 0 .. 10 }
    |> Seq.map (fun x -> (x, 2))
    |> Seq.toList

let roomA = [ (2, 0); (2, 1) ]
let roomB = [ (4, 0); (4, 1) ]
let roomC = [ (6, 0); (6, 1) ]
let roomD = [ (8, 0); (8, 1) ]

let allRooms: Set<Pos> =
    List.concat [ hallway
                  roomA
                  roomB
                  roomC
                  roomD ]
    |> Set

let roomOwners: Map<Pos, Amp> =
    let roomA = roomA |> List.map (fun p -> p, A)
    let roomB = roomB |> List.map (fun p -> p, B)
    let roomC = roomC |> List.map (fun p -> p, C)
    let roomD = roomD |> List.map (fun p -> p, D)

    List.concat [ roomA
                  roomB
                  roomC
                  roomD ]
    |> Map

let ampPos ([ (a1, a2); (b1, b2); (c1, c2); (d1, d2) ]: list<Amp * Amp>) : Map<Pos, Amp> =
    [ (roomA.Head, a1)
      (roomA.Tail.Head, a2)
      (roomB.Head, b1)
      (roomB.Tail.Head, b2)
      (roomC.Head, c1)
      (roomC.Tail.Head, c2)
      (roomD.Head, d1)
      (roomD.Tail.Head, d2) ]
    |> Map

let testAmps =
    ampPos [ (A, B)
             (D, C)
             (C, B)
             (A, D) ]

let prodAmps =
    ampPos [ (D, D)
             (C, A)
             (B, C)
             (B, A) ]

let targetAmps =
    ampPos [ (A, A)
             (B, B)
             (C, C)
             (D, D) ]

let neighbors: Map<Pos, list<Pos>> =
    allRooms
    |> Set.toList
    |> List.map
        (fun (x, y) ->
            (x, y),
            ([ (x + 1, y)
               (x - 1, y)
               (x, y + 1)
               (x, y - 1) ]
             |> List.filter allRooms.Contains))
    |> Map

let ampAtTarget (amps: Map<Pos, Amp>) (pos: Pos) =
    match pos with
    | (x, 0) when roomOwners.TryFind pos = amps.TryFind pos -> true
    | (x, 1) when roomOwners.TryFind pos = amps.TryFind pos -> roomOwners.TryFind(x, 0) = amps.TryFind(x, 0) // below is ok too
    | _ -> false

let notAtTarget (amps: Map<Pos, Amp>) =
    amps
    |> Map.filter (fun pos amp -> ampAtTarget amps pos |> not)

let freeAroundAmp (amps: Map<Pos, Amp>) (pos: Pos) =
    let neighbors = neighbors.TryFind pos |> Option.get

    let free =
        neighbors
        |> List.filter (fun pos -> amps.TryFind pos = None)

    free

type HallwayState =
    | PODFRESH
    | PARKED
    | UNPARKED

type MemoKey = Map<Pos, Amp> * Map<Pos, HallwayState>

type State(amps: Map<Pos, Amp>, cost: int64, lastMoved: Pos, moveStates: Map<Pos, HallwayState>) =
    member this.Amps = amps
    member this.Cost = cost

    member this.MemoKey: MemoKey = (amps, moveStates)

    override this.ToString() =
        let valid = if this.isValid () then "" else "[X]"
        $"State({valid}{amps},{cost} last={lastMoved} moveStates:{moveStates})"

    member this.AvailableMoves() : list<Amp * Pos * Pos> =
        amps
        |> notAtTarget
        |> Map.toList
        |> List.map
            (fun (pos, amp) ->
                freeAroundAmp amps pos
                |> List.map (fun other -> (amp, pos, other)))
        |> List.concat

    member this.ForcedMoves: list<Pos> =
        let blocking =
            amps.Keys
            |> Seq.filter (fun (_, y) -> y = 2)
            |> Seq.filter (fun (x, _) -> x = 2 || x = 4 || x = 6 || x = 8)
            |> Seq.toList

        let unparked =
            moveStates
            |> Map.filter (fun pos hs -> hs = UNPARKED)
            |> Map.keys
            |> Seq.toList

        [ blocking; unparked ] |> List.concat

    member this.ForcedMove() : Option<Pos> =
        if this.ForcedMoves.IsEmpty then
            None
        else
            Some(this.ForcedMoves.Head)

    member this.isValid() : bool = this.ForcedMoves.Length < 2

    member this.Move (amp: Amp) (pos: Pos) (dest: Pos) =
        let (x, y) = pos
        let (_, dy) = dest
        let cost = cost + (stepCost amp)
        let amps = amps.Remove pos
        let amps = amps.Add(dest, amp)

        let moveState =
            match moveStates.TryFind pos with
            | None -> PODFRESH
            | Some (PODFRESH) -> PODFRESH
            | Some (PARKED) -> UNPARKED
            | Some (UNPARKED) -> UNPARKED

        let moveStates =
            moveStates |> Map.map (fun pos hs -> PARKED)

        let moveStates =
            if y = 2 then
                moveStates.Remove(x, y)
            else
                moveStates

        let moveStates =
            if dy = 2 then
                moveStates.Add(dest, moveState)
            else
                moveStates

        State(amps, cost, dest, moveStates)

let amps = testAmps
let initState = State(amps, 0L, nowhere, Map.empty)


let rec atHome (amps: Map<Pos, Amp>) ((x, y): Pos) =
    let inHomeColumn =
        roomOwners.TryFind(x, y) = amps.TryFind(x, y)

    if not inHomeColumn then false
    else if y = 0 then true
    else atHome amps (x, y - 1)

let legalDest (state: State) (amp: Amp) ((x, y): Pos) ((dx, dy): Pos) =
    let home = atHome amps (x, y)

    if home then
        false
    else
        match roomOwners.TryFind(dx, dy) with
        | None -> true // moving into the hallway is ok
        | _ when dy > y -> true // moving up is ok
        | Some (owner) when owner <> amp -> false // moving down into anothers pen is not allowed
        | Some (_) ->
            let below =
                roomOwners.TryFind(dx, dy - 1)
                |> Option.defaultValue amp // counting bottom as self

            below = amp


let nextSteps (state: State) : list<State> =
    if state.isValid () then
        // printfn $"nextSteps ({state})"
        let moves = state.AvailableMoves()
        let forced = state.ForcedMoves
        // printfn $"available: {moves}"
        // printfn $"forced: {forced}"
        let moves: List<Amp * Pos * Pos> =
            if forced.IsEmpty then
                moves
            else
                moves
                |> List.filter (fun (amp, pos, dest) -> pos = forced.Head)

        let moves =
            moves
            |> List.filter (fun (amp, pos, dest) -> legalDest state amp pos dest)

        moves
        |> List.map (fun (amp, pos, dest) -> state.Move amp pos dest)
    else
        []

let state1 = initState.Move B (2, 1) (2, 2)
printfn $"state1={state1}"
printfn $"forced={state1.ForcedMoves}"
let state2 = state1.Move C (4, 1) (4, 2)
printfn $"state2={state2}"
printfn $"forced={state2.ForcedMoves}"

let state3 = state2.Move C (4, 2) (3, 2)
printfn $"state3={state3}"
printfn $"forced={state3.ForcedMoves}"

let state4 = state3.Move C (2, 2) (1, 2)
printfn $"state4={state4}"
printfn $"forced={state4.ForcedMoves}"


let next = nextSteps initState

let steps1 = nextSteps initState
printfn $"steps1 = {steps1}"

type Memo = Map<MemoKey, int64>

let emptyMemo = Map.empty

let rec solve (bestDone: State) (memo: Memo) (states: list<State>) (depth: int) : State =
    if depth > 1000 then
        bestDone
    else
        printfn $"solve {depth} {memo.Count}" // {bestDone} {memo.Count} {states}"

        match states with
        | [] -> bestDone
        | state :: states ->
            let memoValue: int64 =
                memo.TryFind state.MemoKey
                |> Option.defaultValue 100000000L // non-existence is expensive

            if state.Cost > memoValue then
                solve bestDone memo states (depth) // just skipping this
            else
                let memo = memo.Add(state.MemoKey, state.Cost)

                let bestDone =
                    if state.Amps = targetAmps then // finished
                        if state.Cost < bestDone.Cost then
                            state
                        else
                            bestDone
                    else
                        bestDone

                let newStates = nextSteps state
                let states = List.concat [ newStates; states ]
                solve bestDone memo states (depth + 1)

let fakeState =
    State(targetAmps, 1000000000L, nowhere, Map.empty)

let solution =
    solve fakeState emptyMemo [ initState ] 0

printfn $"solution = {solution}"
