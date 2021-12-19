open System.IO

let file = File.ReadAllLines "input.txt" |> Array.toList

type Pos = int*int*int
type Scan = List<Pos> 

type Scanner(id:int,scan:Scan) =
    member this.Id = id 
    member this.Scan = scan
    override this.ToString () = $"Scanner({id} {scan})"

type InputType =
    | TScanner of int
    | TBeacon of Pos  

let rec tokenize (input: list<string>) =
   match input with
   | [] -> [] 
   | "" :: scanner :: rest ->
       let scanner = (scanner.Split ' ').[2] |> int
       TScanner(scanner) :: tokenize rest
   | beacon :: rest ->
       printfn $"beacon {beacon}"
       let xyz = beacon.Split ',' |> Array.map int
       let pos = Pos(xyz.[0],xyz.[1],xyz.[2])
       (TBeacon pos) :: tokenize rest 
      
let fileTokens = tokenize (""::file)
printfn $"fileTokens = {fileTokens}"

type PToken =
    | PBeacons of list<Pos>
    | PScanner of Scanner 

let rec parseTokens (input: List<InputType>) =
    match input with
    | [] -> []
    | t :: rest ->
        match t,parseTokens rest with
        | TBeacon pos,[] -> [PBeacons([pos])]
        | TBeacon pos,PBeacons(poss)::rest ->
            PBeacons(pos::poss) :: rest
        | TBeacon pos,rest -> PBeacons([pos])::rest 
        | TScanner id,PBeacons(poss) :: rest ->
            PScanner(Scanner(id,poss)) :: rest
        | t,rest -> failwith $"Unknown input :: {t} {rest}"

let parse (input:list<string>) =
    let ptokens = tokenize (""::input) |> parseTokens
    ptokens
    |> List.map (fun token ->
                     match token with
                     | PScanner scanner -> scanner
                     | beacon -> failwith $"beacon after parse {beacon}")
let parsed = parse file
printfn $"parsed: {parsed}"
            
        
    