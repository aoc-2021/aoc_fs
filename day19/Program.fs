open System.IO

let file =
    File.ReadAllLines "input.txt" |> Array.toList

type Pos = int * int * int
type Scan = List<Pos>

type Scanner(id: int, scan: Scan) =
    member this.Id = id
    member this.Scan = scan
    override this.ToString() = $"Scanner({id} {scan})"

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
        let xyz = beacon.Split ',' |> Array.map int
        let pos = Pos(xyz.[0], xyz.[1], xyz.[2])
        (TBeacon pos) :: tokenize rest

let fileTokens = tokenize ("" :: file)
printfn $"fileTokens = {fileTokens}"

type PToken =
    | PBeacons of list<Pos>
    | PScanner of Scanner

let rec parseTokens (input: List<InputType>) =
    match input with
    | [] -> []
    | t :: rest ->
        match t, parseTokens rest with
        | TBeacon pos, [] -> [ PBeacons([ pos ]) ]
        | TBeacon pos, PBeacons (poss) :: rest -> PBeacons(pos :: poss) :: rest
        | TBeacon pos, rest -> PBeacons([ pos ]) :: rest
        | TScanner id, PBeacons (poss) :: rest -> PScanner(Scanner(id, poss)) :: rest
        | t, rest -> failwith $"Unknown input :: {t} {rest}"

let parse (input: list<string>) =
    let ptokens = tokenize ("" :: input) |> parseTokens

    ptokens
    |> List.map
        (fun token ->
            match token with
            | PScanner scanner -> scanner
            | beacon -> failwith $"beacon after parse {beacon}")

let parsed = parse file
printfn $"parsed: {parsed}"

let allRotations (x, y, z) : list<Pos> =
    let xyrots (x, y, z) =
        [ (x, y, z)
          (-y, x, z)
          (-x, -y, z)
          (y, -x, z) ]

    let yzrots (x, y, z) =
        [ (x, y, z)
          (x, -z, y)
          (x, -y, -z)
          (x, z, -y) ]

    let xzrots (x, y, z) =
        [ (x, y, z)
          (-z, y, x)
          (-x, y, -z)
          (z, y, -x) ]

    (x, y, z)
    |> xyrots
    |> List.map yzrots
    |> List.concat
    |> List.map xzrots
    |> List.concat

let allRot1 = allRotations (1, 2, 3)
let dRot1 = allRot1 |> List.sort |> List.distinct

printfn $"allRot1 {allRot1} {allRot1.Length} {dRot1} {dRot1.Length}"

dRot1 |> List.map (fun p -> printfn $"{p}")

let rotations ((x, y, z): Pos) =
    [ (-z, -y, -x)
      (-z, -x, y)
      (-z, x, -y)
      (-z, y, x)
      (-y, -z, x)
      (-y, -x, -z)
      (-y, x, z)
      (-y, z, -x)
      (-x, -z, -y)
      (-x, -y, z)
      (-x, y, -z)
      (-x, z, y)
      (x, -z, y)
      (x, -y, -z)
      (x, y, z)
      (x, z, -y)
      (y, -z, -x)
      (y, -x, z)
      (y, x, -z)
      (y, z, x)
      (z, -y, x)
      (z, -x, -y)
      (z, x, y)
      (z, y, -x) ]

type PotentialScanners(scanners:List<Scanner>) =
    member this.Scanners = scanners
    override this.ToString() = $"PotentialScanners({scanners})"
    
let toPotential (scanner:Scanner) : PotentialScanners =
    let posRots = scanner.Scan |> List.map rotations
    let rec toScans (posRots:list<list<Pos>>) : list<Scanner> =
        match posRots with
        | empties when empties.Head.IsEmpty -> []
        | lists ->
            let first = lists |> List.map List.head |> (fun beacons -> Scanner (scanner.Id,beacons))
            let rest = lists |> List.map List.tail |> toScans
            first::rest
    PotentialScanners(toScans posRots)
            
let tScan1 = Scanner(1,[(1,2,3);(2,4,6)])        
let pScan1 = toPotential tScan1

printfn $"pScan1 = {pScan1}"
    