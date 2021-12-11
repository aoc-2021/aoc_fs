open System.Diagnostics.CodeAnalysis
open System.IO
open System 

let input = File.ReadAllLines "input.txt"

let input2 = [|
"5483143223";
"2745854711";
"5264556173";
"6141336146";
"6357385478";
"4167524645";
"2176841721";
"6882881134";
"4846848554";
"5283751526"|]

let lines = input2

let octopuses =
    let toInt c = (int c) - (int '0')
    lines |> Array.map (fun s -> s.ToCharArray () |> Array.map toInt)

type Point = int*int

type Octomap (map:Map<Point,int>) =
    member this.Map = map
    member this.IncByOne () : Octomap =
        map |> Map.map (fun p v -> v+1) |> Octomap
    member this.Charged () : Set<Point> =
        map |> Map.fold (fun nines p v -> if v > 9 then nines.Add p else nines) Set.empty
    member this.Hit (p:Point) : Octomap =
        map |> Map.change p (fun v -> match v with
                                      | Some(x) -> Some(x+1)
                                      | None -> None )
            |> Octomap 
    member this.Defuse () : Octomap =
        map |> Map.map (fun p v -> if v > 9 then 0 else v) |> Octomap 
    override this.ToString () = $"Octomap({map})"
    
    
let allPoints : list<Point> =
    let xs = {0..9}
    let ys = {0..9}
    xs |> Seq.map (fun x -> ys |> Seq.map (fun y -> (x,y)) |> Seq.toList) |> Seq.toList |> List.concat
    
printfn $"all points: {allPoints}"

let buildMap (octopuses:int[][]) : Map<Point,int> =
    let toRef ((x,y):Point) = (x,y),octopuses.[y].[x]
    allPoints |> List.map toRef |> Map
    
let initMap = buildMap octopuses |> Octomap 

printfn $"initMap={initMap}"

let neighbors ((x,y):Point) : List<Point> =
    [(x-1,y-1);(x,y-1);(x+1,y-1);
     (x-1,y);          (x+1,y)
     (x-1,y+1);(x,y+1);(x+1,y+1)]

let step (octos:Octomap) =
    let octos = octos.IncByOne()
    printfn $"inced {octos}"
    let rec flashOff (flashed:Set<Point>) (octos:Octomap) : Octomap =
        let flashers = octos.Charged () |> Set.filter (fun p -> flashed.Contains p |> not)
        if flashers.IsEmpty then octos
        else
            let hits = flashers |> Set.toList |> List.map neighbors |> List.concat
            let octos:Octomap = hits |> List.fold (fun octos -> octos.Hit) octos
            let flashed = Set.union flashed flashers
            flashOff flashed octos
    let octos = flashOff Set.empty octos
    let octos = octos.Defuse ()
    octos 
    
let octos1:Octomap = step initMap 

let octos99 = {1..99} |> Seq.fold (fun octos n -> step octos) initMap 

let stepN (n:int) (octos:Octomap) =
   {1..n} |> Seq.fold (fun octos n -> step octos) octos 

printfn $"99: {stepN 90 initMap}"
