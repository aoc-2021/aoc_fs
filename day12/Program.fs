

// For more information see https://aka.ms/fsharp-console-apps

open System
open System.IO

printfn "Hello from F#"

let file = File.ReadAllLines "input.txt"

let input1 = file |> Array.toList 

type Tube = string*string

let input2 = [
"start-A";
"start-b";
"A-c";
"A-b";
"b-d";
"A-end";
"b-end"]

let input3 = [
"dc-end";
"HN-start";
"start-kj";
"dc-start";
"dc-HN";
"LN-dc";
"HN-end";
"kj-sa";
"kj-HN";
"kj-dc"]

let input = input1

let allTubesForward: Set<Tube> = input |> List.map (fun s -> s.Split '-' |> fun s -> (s.[0],s[1])) |> Set
let allTubesBackward: Set<Tube> = allTubesForward |> Set.map (fun (a,b) -> (b,a))

let allTubes = Set.union allTubesForward allTubesBackward

// printfn $"tubes: ${allTubes}"

let starts = allTubes |> Set.filter (fun (b,_) -> b = "start")

let tubes = allTubes |> Set.filter (fun (a,b) -> a = "start" || b = "start" |> not) 

let goesToSmall (tube:Tube) =
    let cave = snd tube
    if cave = "end" then false
    else cave.ToCharArray () |> Array.filter Char.IsUpper |> Array.isEmpty

let rec expand (visitedTwice:bool) (visitedSmall:Set<string>) (rest:Set<Tube>) (tube:Tube) =
    let secondVisit = goesToSmall tube && visitedSmall.Contains (snd tube)
    let visitedSmall = if goesToSmall tube then visitedSmall.Add (snd tube) else visitedSmall 
    if secondVisit && visitedTwice then []
    else if snd tube = "end" then [[tube]]
    else
        let visitedTwice = visitedTwice || secondVisit
        printfn $"expand {visitedSmall} {rest} {tube}"
        let continuesTube fromTube tube = snd fromTube = fst tube  
        let nexts : List<Tube> = rest |> Set.filter (continuesTube tube) |> Set.toList 
        printfn $"tube={tube} nexts={nexts}"
        let nextPaths : List<List<Tube>> = nexts |> List.map (expand visitedTwice visitedSmall rest) |> List.concat
        let paths = nextPaths |> List.map (fun path -> tube::path)
        paths 
         
let paths = starts |> Set.toList |> List.map (expand false Set.empty tubes) |> List.concat 

printfn $"paths: {paths.Length} {paths}"

let rec pathToCaves (path:List<Tube>) =
    match path with
    | [] -> ["end"]
    | (start,_)::path -> start::(pathToCaves path)
let sPaths = paths |> List.map pathToCaves |> List.map (fun p -> p |> String.concat ",")

printfn $"paths: {paths.Length} {sPaths}"

// sPaths |> List.map (fun s -> printfn $"{s}") 

let noBiDirs (path:List<Tube>) =
    let tubes = path |> Set 
    let rev (a,b) = (b,a)
    let revs = tubes |> Set.map rev 
    let bidirs = Set.intersect tubes revs
    bidirs.IsEmpty

let isValidPath (path:List<Tube>) = path |> List.filter goesToSmall |> List.length < 2

let validPaths = paths |> List.filter isValidPath

printfn $"Task 1: {validPaths.Length} {validPaths}"
