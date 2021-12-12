

// For more information see https://aka.ms/fsharp-console-apps

open System
open System.IO

printfn "Hello from F#"

let file = File.ReadAllLines "input.txt"

let input = file |> Array.toList 

type Tube = string*string 
let input2 = [
"start-A";
"start-b";
"A-c";
"A-b";
"b-d";
"A-end";
"b-end"]

let allTubes : Set<Tube> = input |> List.map (fun s -> s.Split '-' |> fun s -> (s.[0],s[1])) |> Set

printfn $"tubes: ${allTubes}"

let starts = allTubes |> Set.filter (fun (b,e) -> b = "start")

let tubes = starts |> Set.fold (fun rest tube -> Set.remove tube rest) allTubes 


let rec expand (rest:Set<Tube>) (tube:Tube) =
    printfn $"expand {rest} {tube}"
    let rest = rest.Remove tube 
    if snd tube = "end" then [[tube]]
    else
        let continuesTube fromTube tube = snd fromTube = fst tube  
        let nexts : List<Tube> = rest |> Set.filter (continuesTube tube) |> Set.toList 
        printfn $"tube={tube} nexts={nexts}"
        let nextPaths : List<List<Tube>> = nexts |> List.map (expand rest) |> List.concat
        let paths = nextPaths |> List.map (fun path -> tube::path)
        printfn $"nextPaths={nextPaths}"
        printfn $"paths={paths}" 
        paths 
         
let paths = starts |> Set.toList |> List.map (expand tubes) |> List.concat 

printfn $"paths: {paths}"

let goesToSmall (tube:Tube) =
    let cave = snd tube
    if cave = "end" then false
    else cave.ToCharArray () |> Array.filter Char.IsUpper |> Array.isEmpty

let isValidPath (path:List<Tube>) = path |> List.filter goesToSmall |> List.length < 2

let validPaths = paths |> List.filter isValidPath

printfn $"Task 1: {validPaths.Length} {validPaths}"
