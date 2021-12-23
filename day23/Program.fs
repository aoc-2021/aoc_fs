open System.IO

let file = File.ReadAllLines "input.txt"

type Amp = A | B | C | D
let stepCost (amp:Amp) =
    match amp with
    | A -> 1
    | B -> 10
    | C -> 20
    | D -> 30

type Pos = int*int

let hallway = {0..10} |> Seq.map (fun x -> (x,2)) |> Seq.toList 
let roomA = [(2,0);(2,1)]
let roomB = [(4,0);(4,1)]
let roomC = [(6,0);(6,1)]
let roomD = [(8,0);(8,1)]

let allRooms : Set<Pos> = List.concat [hallway;roomA;roomB;roomC;roomD] |> Set 
let roomOwners : Map<Pos,Amp> =
    let roomA = roomA |> List.map (fun p -> p,A)
    let roomB = roomB |> List.map (fun p -> p,B)
    let roomC = roomC |> List.map (fun p -> p,C)
    let roomD = roomD |> List.map (fun p -> p,D)
    List.concat [roomA;roomB;roomC;roomD] |> Map 

let ampPos ([(a1,a2);(b1,b2);(c1,c2);(d1,d2)] :list<Amp*Amp>) : Map<Pos,Amp> =
    [(roomA.Head,a1);
     (roomA.Tail.Head,a2);
     (roomB.Head,b1);
    (roomB.Tail.Head,b2);
    (roomC.Head,c1);
    (roomC.Tail.Head,c2);
    (roomD.Head,d1);
    (roomD.Tail.Head,d2)] |> Map 
 
let testAmps = ampPos [(A,B);(D,C);(C,B);(A,D)]
let prodAmps = ampPos [(D,D);(C,A);(B,C);(B,A)]

let targetAmps = ampPos [(A,A);(B,B);(C,C);(D,D)]

let neighbors :Map<Pos,list<Pos>> =
    allRooms
    |> Set.toList
    |> List.map (fun (x,y) -> (x,y),([(x+1,y);(x-1,y);(x,y+1);(x,y-1)]
                                     |> List.filter allRooms.Contains))
    |> Map 

let ampAtTarget (amps:Map<Pos,Amp>) (pos:Pos) =
    match pos with
    | (x,0) when roomOwners.TryFind pos = amps.TryFind pos -> true 
    | (x,1) when roomOwners.TryFind pos = amps.TryFind pos ->
        roomOwners.TryFind (x,0) = amps.TryFind (x,0) // below is ok too 
    | _ -> false
    
let notAtTarget (amps:Map<Pos,Amp>) =
    amps |> Map.filter (fun pos amp -> ampAtTarget amps pos |> not) 
       
let freeAroundAmp (amps:Map<Pos,Amp>) (pos:Pos) =
    let neighbors = neighbors.TryFind pos |> Option.get
    let free = neighbors |> List.filter (fun pos -> amps.TryFind pos = None)
    printfn $"freeAroundAmp: {pos} -> {free} neighbors={neighbors}"
    free

type State (amps:Map<Pos,Amp>,cost:int64) =
    member this.Amps = amps
    member this.Cost = cost
    override this.ToString () = $"State({amps},{cost})"
    
    member this.AvailableMoves () : list<Amp*Pos*Pos> =
            amps |> notAtTarget
                 |> Map.toList
                 |> List.map (fun (pos,amp) ->
                     freeAroundAmp amps pos
                     |> List.map (fun other -> (amp,pos,other)))
                 |> List.concat 
          
let amps = testAmps
let initState = State(amps,0L)

let initMoves = initState.AvailableMoves ()
printfn $"initState: {initState}"
printfn $"available: {initMoves}"

