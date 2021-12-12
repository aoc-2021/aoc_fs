open System
open System.IO

let input = File.ReadAllLines "input.txt" |> Array.toList 

type Cave = string
type Tube = Cave*Cave

let allTubesForward: Set<Tube> = input |> List.map (fun s -> s.Split '-' |> fun s -> (s.[0],s[1])) |> Set
let allTubesBackward: Set<Tube> = allTubesForward |> Set.map (fun (a,b) -> (b,a))

let allTubes = Set.union allTubesForward allTubesBackward

let starts = allTubes |> Set.filter (fun (b,_) -> b = "start")

let tubes = allTubes |> Set.filter (fun (a,b) -> a = "start" || b = "start" |> not) 

let isSmall (cave:Cave) = cave.ToCharArray () |> Array.exists Char.IsUpper |> not 

let rec expand (denySecondVisits:bool) (visitedSmall:Set<string>) (rest:Set<Tube>) ((s,e):Tube) =
    let secondVisit = isSmall e && visitedSmall.Contains e
    let visitedSmall = if goesToSmall (s,e) then visitedSmall.Add e else visitedSmall 
    if secondVisit && denySecondVisits then []
    else if e = "end" then [[(s,e)]]
    else
        let denySecondVisits = denySecondVisits || secondVisit
        let continuesTube fromTube tube = snd fromTube = fst tube  
        let nexts : List<Tube> = rest |> Set.filter (continuesTube (s,e)) |> Set.toList 
        let nextPaths : List<List<Tube>> = nexts |> List.map (expand denySecondVisits visitedSmall rest) |> List.concat
        let paths = nextPaths |> List.map (fun path -> (s,e)::path)
        paths 
         
let paths1 = starts |> Set.toList |> List.map (expand true Set.empty tubes) |> List.concat 
let paths2 = starts |> Set.toList |> List.map (expand false Set.empty tubes) |> List.concat 

printfn $"Task 1: {paths1.Length}" // correct is 3463
printfn $"Task 2: {paths2.Length}" // correct is 91533