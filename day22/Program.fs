open System.IO

let file = File.ReadAllLines "input.txt"
let testFile1 = File.ReadAllLines "testInput1.txt"
let testFile2 = File.ReadAllLines "testInput2.txt"

let splitCoord (line:string) =
    let p1 = line.Split '='
    let axis = p1.[0]
    let line = p1.[1]
    let p1 = line.Split '.'
    let first = p1.[0]
    let last = p1.[2]
    // printfn $"axis={axis} {first}->{last}"
    (axis,(first |> int64,last |> int64))
    

let parseLine (line:string) =
    let p1 = line.Split ' '
    let toggle = p1.[0]
    let line = p1.[1]
    let p2 = line.Split ','
    let axi : Map<string,int64*int64> = p2 |> Array.map splitCoord |> Map
    (toggle,axi)
     
let line = parseLine "on x=-48..6,y=-13..40,z=-12..35"
printfn $"{line}"

type Point = int64*int64*int64
type Cuboid(minPos:Point,maxPos:Point) =
    let (x1,y1,z1) = minPos
    let (x2,y2,z2) = maxPos
    let within50 = (x1 > 50L || x2 < -50L || y1 > 50L || y2 < -50L || z1 > 50L || z2 < -50L) |> not 
    member this.MinPos = minPos
    member this.MaxPos = maxPos
    override this.ToString() =
        $"Cuboid({minPos}->{maxPos}"
    member this.Contains ((x,y,z):Point) =
        x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2
        
    member this.ToCuboid50 () : Option<Cuboid> =
        if within50 then
            let x1 = max x1 -50L
            let x2 = min x2 50L
            let y1 = max y1 -50L
            let y2 = min y2 50L
            let z1 = max z1 -50L
            let z2 = min z2 50L
            let minPos = (x1,y1,z1)
            let maxPos = (x2,y2,z2)
            Some(Cuboid(minPos,maxPos))
        else
            None
    member this.allPos () : list<Point> =
        Seq.allPairs (Seq.allPairs {x1..x2} {y1..y2}) {z1..z2}
        |> Seq.map (fun ((x,y),z) -> (x,y,z))
        |> Seq.toList
    member this.IntersectsWith (other:Cuboid) =
        let (ox1,oy1,oz1) = other.MinPos
        let (ox2,oy2,oz2) = other.MaxPos
        (ox1 > x2 || ox2 < x1 ||
         oy1 > y2 || oy2 < y1 ||
         oz1 > z2 || oz2 < z1) 
        
    member this.IntersectOnCuboid (other:Cuboid) : Option<Cuboid>*List<Cuboid> =
        let (ox1,oy1,oz1) = other.MinPos
        let (ox2,oy2,oz2) = other.MaxPos
        if this.IntersectsWith other then
            let cx1 = max x1 ox1
            let cx2 = min x2 ox2
            let cy1 = max y1 oy1
            let cy2 = min y2 oy2
            let cz1 = max z1 oz1
            let cz2 = min z2 oz2
            let inter = Cuboid((cx1,cy1,cz1),(cx2,cy2,cz2))
            let hasLeft = x1 < ox1
            let hasRight = x2 > ox2
            let hasBottom = y1 < oy1
            let hasTop = y2 > oy2
            let hasFront = z1 < oz1
            let hasBack = z2 > oz2
            let rest : list<Cuboid> = []
            let rest =
                if hasTop then
                    let top = Cuboid((x1,oy2,z1),(x2,y2,z2))
                    top :: rest 
                else rest
            let rest =
                if hasBottom then
                    let bottom = Cuboid((x1,y1,z1),(x2,oy1,z2))
                    bottom :: rest
                else rest
            let y1 = max y1 oy1 // raising the floor
            let y2 = min y2 oy2 // lowering the ceiling
            let rest =
                if hasLeft then
                    let left = Cuboid((x1,y1,z1),(ox1,y2,z2))
                    left :: rest
                else rest
            let rest =
                if hasRight then
                    let right = Cuboid((ox2,y1,z1),(x2,y2,z2))
                    right :: rest
                else rest
            let x1 = max x1 ox1 // cutting off left 
            let x2 = min x2 ox2 // cutting off right
            let rest =
                if hasFront then
                    let front = Cuboid((x1,y1,z1),(x2,y2,oz1))
                    front :: rest
                else rest
            let rest =
                if hasBack then
                    let back = Cuboid((x1,y1,oz2),(x2,y2,z2))
                    back :: rest
                else rest
            Some(inter),rest  
        else
            None,[this]
        
type CubeState = ON | OFF 

let toCubeState (s:string) =
    match s with
    | "on" -> ON
    | "off" -> OFF
    | _ -> failwith $"Unknown toggle: {s}"

type Command(toggle:CubeState,cube:Cuboid) =
    member this.Toggle = toggle
    member this.Cube = cube
    override this.ToString() = $"Command({toggle} {cube})"
    member this.ToCommand50 () : Option<Command> =
        cube.ToCuboid50 ()
        |> Option.map (fun cube50 -> Command(toggle,cube50))
    member this.ToPointStates () =
        cube.allPos ()
        |> List.map (fun pos -> (pos,toggle))
    
let toCuboid (line:string) =
    let (toggleS,axisMap) = parseLine line
    let toggle = toCubeState toggleS
    let x : int64*int64 = axisMap |> Map.tryFind "x" |> Option.get
    let y : int64*int64 = axisMap |> Map.tryFind "y" |> Option.get
    let z : int64*int64 = axisMap |> Map.tryFind "z" |> Option.get
    let minPos = (fst x, fst y, fst z)
    let maxPos = (snd x, snd y, snd z)
    let cuboid = Cuboid(minPos,maxPos)
    Command(toggle,cuboid)
    
// let command = toCube "on x=-48..6,y=-13..40,z=-12..35"
// printfn $"{command}"

let prodCommands = file |> Array.map toCuboid |> Array.toList
let testCommands1 = testFile1 |> Array.map toCuboid |> Array.toList

let testCommands2 = testFile2 |> Array.map toCuboid |> Array.toList

// let commands = testCommands1
let commands = prodCommands


// Naive solution

let allPos50 =
    Seq.allPairs (Seq.allPairs {-50L..50L} {-50L..50L}) {-50L..50L}
    |> Seq.map (fun ((x,y),z) -> (x,y,z))
    |> Seq.toList 

let commands50 = commands
                 |> List.map (fun command -> command.ToCommand50 ())
                 |> List.filter Option.isSome
                 |> List.map Option.get 

printfn $"commands: {commands.Length} commands50: {commands50.Length}"

let stateMap50 = allPos50 |> List.map (fun pos -> (pos,OFF)) |> Map 

let c1 = commands50.Head
let applyState (states:Map<Point,CubeState>) (command:Command) : Map<Point,CubeState> =
    let addPos (states:Map<Point,CubeState>) (pointState:Point*CubeState) =
        states.Add pointState
    command.ToPointStates ()
    |> List.fold addPos states 
    
let state1 = applyState stateMap50 c1
let state2 = applyState state1 commands50.Tail.Head 
let state3 = applyState state2 commands50.Tail.Tail.Head 
let toggled1 = state1 |> Map.filter (fun key value -> value = ON)
let toggled2 = state2 |> Map.filter (fun key value -> value = ON)
let toggled3 = state3 |> Map.filter (fun key value -> value = ON)

printfn 
printfn $"c1={c1}"
toggled1.Keys |> Seq.map (fun p -> printfn $"{p}") |> Seq.toList 

printfn $"toggled1 = {toggled1.Count}"
printfn $"toggled2 = {toggled2.Count}"
printfn $"toggled3 = {toggled3.Count}"

let finalState50 = commands50 |> List.fold applyState stateMap50
let finalOns50 = finalState50 |> Map.filter (fun key value -> value = ON) |> Map.keys 

printfn $"finalOns50: {finalOns50 |> Seq.length}"

// part 2







