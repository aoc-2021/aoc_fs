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
type Cube(minPos:Point,maxPos:Point) =
    member this.MinPos = minPos
    member this.MaxPos = maxPos
    override this.ToString() =
        $"Cube({minPos}->{maxPos}"

type CubeState = ON | OFF 

let toCubeState (s:string) =
    match s with
    | "on" -> ON
    | "off" -> OFF
    | _ -> failwith $"Unknown toggle: {s}"

type Command(toggle:CubeState,cube:Cube) =
    member this.Toggle = toggle
    member this.Cube = cube
    override this.ToString() = $"Command({toggle} {cube})"
    
let toCube (line:string) =
    let (toggleS,axisMap) = parseLine line
    let toggle = toCubeState toggleS
    let x : int64*int64 = axisMap |> Map.tryFind "x" |> Option.get
    let y : int64*int64 = axisMap |> Map.tryFind "y" |> Option.get
    let z : int64*int64 = axisMap |> Map.tryFind "z" |> Option.get
    let minPos = (fst x, fst y, fst z)
    let maxPos = (snd x, snd y, snd z)
    let cube = Cube(minPos,maxPos)
    Command(toggle,cube)
    
// let command = toCube "on x=-48..6,y=-13..40,z=-12..35"
// printfn $"{command}"

let prodCommands = file |> Array.map toCube |> Array.toList
let testCommands1 = testFile1 |> Array.map toCube |> Array.toList

let testCommands2 = testFile2 |> Array.map toCube |> Array.toList

let commands = testCommands1 


