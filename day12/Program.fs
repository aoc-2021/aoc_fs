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

type CaveMap = Map<Cave,list<Tube>>
let caveMap : CaveMap = tubes |> Set.toList |> List.groupBy fst |> Map 

let isSmall (cave:Cave) = cave.ToCharArray () |> Array.exists Char.IsUpper |> not 

let rec expand (denySecondVisits:bool) (visitedSmall:Set<string>) (caveMap:CaveMap) ((s,e):Tube) =
    let secondVisit = isSmall e && visitedSmall.Contains e
    let visitedSmall = if isSmall e then visitedSmall.Add e else visitedSmall 
    if secondVisit && denySecondVisits then []
    else if e = "end" then [[(s,e)]]
    else
        let denySecondVisits = denySecondVisits || secondVisit
        let nexts = caveMap.TryFind e |> Option.defaultValue []
        let nextPaths : List<List<Tube>> = nexts |> List.map (expand denySecondVisits visitedSmall caveMap) |> List.concat
        let paths = nextPaths |> List.map (fun path -> (s,e)::path)
        paths 
         
let paths1 = starts |> Set.toList |> List.map (expand true Set.empty caveMap) |> List.concat 
let paths2 = starts |> Set.toList |> List.map (expand false Set.empty caveMap) |> List.concat 

printfn $"Task 1: {paths1.Length}" // correct is 3463
printfn $"Task 2: {paths2.Length}" // correct is 91533