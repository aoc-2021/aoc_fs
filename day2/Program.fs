open System
open System.IO

let readFile (fileName:String) : seq<String> = seq {
    use sr = new StreamReader(fileName)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitLine (s:string) : array<string> = s.Split (' ')
let toDelta (c:array<String>) =
    match (c.[0]) with
    | "forward" -> ((c.[1] |> int),0)
    | "up" -> (0,-(c.[1] |> int))
    | "down" -> (0, (c.[1]|> int))

let commands = readFile "input.txt" |> Seq.map splitLine |> Seq.toArray 
let deltas = commands |> Array.map toDelta 
let moveAndAims = deltas |> Seq.toList 

let depth ((x,y)) = y
let forward ((x,y)) = x 

let depths = deltas |> Array.map depth |> Array.sum 
let forwards = deltas |> Array.map forward |> Array.sum 

printfn $"commands: %A{commands}"
printfn $"deltas: %A{deltas}"
        
printfn $"Answer: %A{depths*forwards}"

let adjustPos ((currFor,currDepth,currAim)) (forward,aimDelta) =
    let newForward = currFor + forward
    let newDepth = currDepth + (forward*currAim)
    let newAim = currAim + aimDelta
    (newForward,newDepth,newAim)
    
let rec adjustPositions (d) (x:list<Tuple<int,int>>) =
    if x.IsEmpty then d
    else
        let newPos = adjustPos d (x.Head)
        adjustPositions newPos x.Tail
        
let finalPos = adjustPositions (0,0,0) moveAndAims

let toAnswer (a,b,_) = (a |> int64) * (b |> int64)
let answer = toAnswer finalPos

printfn $"Newpos: {finalPos} {answer}"