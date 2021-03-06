open System.IO

let input = File.ReadAllLines "input.txt"

let octopuses =
    let toInt c = (int c) - (int '0')
    input |> Array.map (fun s -> s.ToCharArray () |> Array.map toInt)

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
    
let buildMap (octopuses:int[][]) : Map<Point,int> =
    let toRef ((x,y):Point) = (x,y),octopuses.[y].[x]
    allPoints |> List.map toRef |> Map
    
let initOctos = buildMap octopuses |> Octomap 

let neighbors ((x,y):Point) : List<Point> =
    [(x-1,y-1);(x,y-1);(x+1,y-1);
     (x-1,y);          (x+1,y)
     (x-1,y+1);(x,y+1);(x+1,y+1)]

let step (octos:Octomap) : Octomap*int =
    let octos = octos.IncByOne()
    let rec flashOff (flashed:Set<Point>) (octos:Octomap) : Octomap*int =
        let flashers = octos.Charged () |> Set.filter (fun p -> flashed.Contains p |> not)
        if flashers.IsEmpty then octos,flashed.Count
        else
            let hits = flashers |> Set.toList |> List.map neighbors |> List.concat
            let octos:Octomap = hits |> List.fold (fun octos -> octos.Hit) octos
            let flashed = Set.union flashed flashers
            flashOff flashed octos
    let octos,flashCount = flashOff Set.empty octos
    let octos = octos.Defuse ()
    octos,flashCount 
    
let octos1,flashCount = step initOctos 

let octos99 = {1..99} |> Seq.fold (fun (octos,c) n ->
    let (nextOctos,newFlashes) = step octos
    (nextOctos,newFlashes+c)) (initOctos,0)

let stepN (n:int) (octos:Octomap) =
   {1..n} |> Seq.fold (fun (octos,c) n ->
       let (octos,newFlashes) = step octos
       octos,newFlashes+c) (octos,0)
   
printfn $"Task 1 {stepN 100 initOctos |> snd}"

// part 2
let rec findFullFlash (octos:Octomap) (stepcount:int) : int =
    let stepcount = stepcount + 1
    let octos,flashes = step octos
    if flashes < 100 then
        findFullFlash octos stepcount
    else 
        stepcount

let brightStep = findFullFlash initOctos 0
printfn $"Task 2: {brightStep}"