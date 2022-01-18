open System.IO

let input = File.ReadAllLines "input.txt"
let testInput = File.ReadAllLines "testInput.txt"

type Thing =
    | EAST
    | SOUTH

let t2s (thing:Thing) =
    match thing with
    | EAST -> "▶"
    | SOUTH -> "▼"

type Point = int*int

let read (lines:string[]): list<Thing*(int*int)> =
    let toThing (c:char) =
        match c with
        | 'v' -> Some(SOUTH)
        | '>' -> Some(EAST)
        | '.' -> None
        | _ -> failwith $"Not recognized char: {c}"
    let readLine (lineNo:int) (line:string) =
        let line = line.ToCharArray ()
        let columns = [0..line.Length-1]
        columns |> List.map (fun i -> line[i] |> toThing |> Option.map (fun thing -> thing,(i,lineNo)))
                |> List.filter Option.isSome
                |> List.map Option.get
    let ys = [0..lines.Length-1]
    ys |> List.map (fun y -> readLine y lines[y]) |> List.concat 
                
type Area(easterners:Set<Point>,southerners:Set<Point>,east:int,south:int) =
    member this.Things () =
        let easts = easterners |> Set.toList |> List.map (fun p -> p,EAST)
        let souths = southerners |> Set.toList |> List.map (fun p -> p,SOUTH)
        let all = [easts;souths] |> List.concat
        all |> Map
    member this.Easterners = easterners
    member this.Southerners = southerners
    
    member this.MoveEast() =
        let isBusy p = (easterners.Contains p || southerners.Contains p)
        let move (toNext:Point->Point) (p:Point) =
            let next = toNext p
            if isBusy next then
                // printfn $"{p}->busy:{next}->{p}"
                p
            else
                // printfn $"{p}->{next}"
                next 
        let easterners = easterners |> Set.map (move (fun (x,y) -> ((x+1)%east),y))
        Area(easterners,southerners,east,south)

    member this.MoveSouth() =
        let orig = this
        let isBusy p = (easterners.Contains p || southerners.Contains p)
        let move (toNext:Point->Point) (p:Point) =
            let next = toNext p
            if isBusy next then
                // printfn $"{p}->busy:{next}->{p}"
                p
            else
                // printfn $"{p}->{next}"
                next 
        let southerners = southerners |> Set.map (move (fun (x,y) -> (x,(y+1)%south)))
        Area(easterners,southerners,east,south)
    
    member this.Move () =
        let area = this.MoveEast()
        area.MoveSouth()
     
    override this.ToString() =
        $"Area({this.Things ()} [{(east,south)}]"
                                                  

let toArea (file:string) =
    let file = File.ReadAllLines file
    let parse file : list<Point*Thing> = file |> read |> List.map (fun (a,b) -> b,a)
    let things = parse file
    let south = file.Length
    let east = file[0].Length
    let easterners = things |> List.filter (snd >> (=) EAST) |> List.map fst |> Set
    let southerners = things |> List.filter (snd >> (=) SOUTH) |> List.map fst |> Set
    Area(easterners,southerners,east,south)
let prodArea = toArea "input.txt"
let testArea = toArea "testInput.txt"

let rec moveUntilStuck (n:int) (area:Area) =
        let next = area.Move()
        let stuck = area.Southerners = next.Southerners & area.Easterners = next.Easterners
        if stuck then n
        else 
            moveUntilStuck (n+1) (area.Move())

let n = moveUntilStuck 1 testArea

printfn $"stuck at {n}"

let m = moveUntilStuck 1 prodArea

printfn $"stuck at {m}"
