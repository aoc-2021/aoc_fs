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
        |> not 
    member this.Volume () : int64  =
        let (x1,y1,z1) = minPos
        let (x2,y2,z2) = maxPos
        let dx = x2-x1+1L
        let dy = y2-y1+1L
        let dz = z2-z1+1L 
        dx*dy*dz 
        
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
                    let top = Cuboid((x1,oy2+1L,z1),(x2,y2,z2))
                    top :: rest 
                else rest
            let rest =
                if hasBottom then
                    let bottom = Cuboid((x1,y1,z1),(x2,oy1-1L,z2))
                    bottom :: rest
                else rest
            let y1 = max y1 oy1 // raising the floor
            let y2 = min y2 oy2 // lowering the ceiling
            let rest =
                if hasLeft then
                    let left = Cuboid((x1,y1,z1),(ox1-1L,y2,z2))
                    left :: rest
                else rest
            let rest =
                if hasRight then
                    let right = Cuboid((ox2+1L,y1,z1),(x2,y2,z2))
                    right :: rest
                else rest
            let x1 = max x1 ox1 // cutting off left 
            let x2 = min x2 ox2 // cutting off right
            let rest =
                if hasFront then
                    let front = Cuboid((x1,y1,z1),(x2,y2,oz1-1L))
                    front :: rest
                else rest
            let rest =
                if hasBack then
                    let back = Cuboid((x1,y1,oz2+1L),(x2,y2,z2))
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
let commands = testCommands1

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

let finalState50 = commands50 |> List.fold applyState stateMap50
let finalOns50 = finalState50 |> Map.filter (fun key value -> value = ON) |> Map.keys 

printfn $"finalOns50: {finalOns50 |> Seq.length}"

// part 2

let testCuboidLarge = Cuboid ((0,0,0),(5,6,7))
let testCuboidSmall = Cuboid ((1,2,3),(4,5,6))

let intSelf = testCuboidLarge.IntersectsWith testCuboidLarge
let intSmall = testCuboidLarge.IntersectsWith testCuboidSmall

printfn $"int self={intSelf} small={intSmall}"



printfn ""
printfn ""
printfn ""

let testFull1 = testCuboidLarge.IntersectOnCuboid testCuboidLarge // 1:1 match
printfn $"testFull1 = {testFull1}"
let testSplit1 = testCuboidLarge.IntersectOnCuboid testCuboidSmall
printf $"testSplit1 = {testSplit1}"

printfn ""
printfn ""
printfn ""
printfn ""

let (c,r) = testSplit1
let ov1 = c |> Option.get
let v1 = ov1.Volume ()
let v2 = r |> List.map (fun c -> c.Volume ()) |> List.sum 

let splitVolume = v1 + v2
let origVolume = testCuboidLarge.Volume () 

printfn $"volume {origVolume} -> {splitVolume}"

let findHugeCube (commands:list<Command>) =
    let mins = commands |> List.map (fun com -> com.Cube.MinPos)
    let maxs = commands |> List.map (fun com -> com.Cube.MaxPos)
    let toX ((x,y,z):Point) = x
    let toY ((x,y,z):Point) = y
    let toZ ((x,y,z):Point) = z
    let minX = mins |> List.map toX |> List.min
    let minY = mins |> List.map toY |> List.min
    let minZ = mins |> List.map toZ |> List.min
    let maxX = maxs |> List.map toX |> List.max
    let maxY = maxs |> List.map toY |> List.max
    let maxZ = maxs |> List.map toZ |> List.max
    Cuboid ((minX,minY,minZ),(maxX,maxY,maxZ))

let startCuboids:list<Cuboid*CubeState> = [findHugeCube commands,OFF]

let applyCommandSingle ((cuboid,state):Cuboid*CubeState) (command:Command):List<Cuboid*CubeState> =
    if state = command.Toggle then [(cuboid,state)] // no point in toggling to same 
    else
        match command.Cube.IntersectOnCuboid cuboid with
        | None,_ -> [(cuboid,state)] 
        | Some(inter),rest ->
            let (_,outside) = cuboid.IntersectOnCuboid command.Cube 
            let outside = outside |> List.map (fun o -> o,state) 
            printfn $"applyCommand: split into {inter} {rest}"
            let rest = rest |> List.map (fun c -> c,state) // rest keeps the state
            [(inter,command.Toggle) :: rest ; outside] |> List.concat 
         
let applyCommand (cubestates:list<Cuboid*CubeState>) (command:Command) =
    cubestates |> List.map (fun cs -> applyCommandSingle cs command) |> List.concat
    
let cubes1 = applyCommand startCuboids commands.Head

let endCubes = commands |> List.fold applyCommand startCuboids 

printfn $"start={startCuboids}" 
printfn $"cubes1={cubes1}"
printfn $"endCubes={endCubes}"
let ons = endCubes |> List.filter (fun (_,state) -> state = ON) |> List.map fst

endCubes |> List.map (fun c -> printfn $"*cube: {c}")

printfn $"ons = {ons}"

printfn $"Volume (1,1,1)(1,1,1) = {Cuboid((1,1,1),(3,3,3)).Volume ()}"

let lit = ons |> List.map (fun on -> on.Volume ()) |> List.sum

printfn $"lit={lit}"

