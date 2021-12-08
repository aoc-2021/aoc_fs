
open System
open System.IO
open System.Text.RegularExpressions

let file = File.ReadAllLines "input.txt"

let outs = file |> Array.map (fun s -> s.Split '|')
                |> Array.map (fun s -> s.[1])
                |> Array.map (fun (s:string) -> s.Trim ' ')
                |> Array.map (fun s -> s.Split ' ' |> Array.map (String.length) |> Array.toList)
                |> Array.toList
let outs2 = List.concat outs |> List.groupBy id
            |> List.map (fun (a,b) -> (a,b.Length))
            |> Map

let ones = outs2.TryFind 2 |> Option.defaultValue -1
let fours = outs2.TryFind 4 |> Option.defaultValue -1
let sevens = outs2.TryFind 3 |> Option.defaultValue -1
let eights = outs2.TryFind 7 |> Option.defaultValue -1

// printfn $"{ones + fours + sevens + eights}"

// printfn $"outs = ${outs2}"


let segsForDigit (digit:int) =
            match digit with
            | 0 -> ['A';'B';'C';'E';'F';'G'] |> Set
            | 1 -> ['C';'F'] |> Set
            | 2 -> ['A';'C';'D';'E';'G'] |> Set
            | 3 -> ['A';'C';'D';'F';'G'] |> Set
            | 4 -> ['B';'C';'D';'F'] |> Set
            | 5 -> ['A';'B';'D';'F';'G'] |> Set
            | 6 -> ['A';'B';'D';'E';'F';'G'] |> Set
            | 7 -> ['A';'C';'F'] |> Set
            | 8 -> ['A';'B';'C';'D';'E';'F';'G'] |> Set
            | 9 -> ['A';'B';'C';'D';'F';'G'] |> Set
            | d -> failwith $"invalid digit ${d}"

let lengthToPossibleSegments (length:int) : Set<char> =
    match length with
    | 2 -> segsForDigit 1 
    | 3 -> segsForDigit 7
    | 4 -> segsForDigit 4
    | 5 -> [2;3;5] |> List.map segsForDigit |> Set.unionMany
    | 6 -> [0;6;9] |> List.map segsForDigit |> Set.unionMany
    | 7 -> segsForDigit 8
    | l -> failwith $"invalid segment count {l}"

type CDigit(wires:string) =
    member this.Wires = wires.ToCharArray () |> Set 
    override this.ToString () = wires
    member this.PossibleSegments =
        // printfn $"wires = {wires}"
        wires.Length |> lengthToPossibleSegments
    
type CLine(allDigits:list<CDigit>,answer:list<CDigit>) =
    member this.AllDigits = allDigits
    member this.Answer = answer
    override this.ToString () = $"CLine({allDigits} | {answer})"

let parseLine (s:string) =
    let out = ((s.Split '|').[1].Trim ' ').Split ' ' |> Array.map CDigit |> Array.toList 
    let all = (Regex.Replace (s," \\| "," ")).Split ' ' |> Array.map CDigit |> Array.toList 
    CLine(all,out)
    
let clines = file |> Array.map parseLine

// printfn $"{clines |> Array.toList}"

let allPos = ['A';'B';'C';'D';'E';'F';'G'] |> Set 
let allCandidates = ['a'..'g'] |> List.map (fun w -> (w,allPos)) |> Map

let filterCandidates (cands:Map<char,Set<char>>) (digit:CDigit) =
    let x = 1
    let wires = digit.Wires
    let possibles = digit.PossibleSegments
    let newCands = cands |> Map.toList
                   |> List.map (fun (c,cands) ->
                                if wires.Contains c
                                then (c, Set.intersect cands possibles)
                                else (c,cands))
                   |> Map
    newCands 

let xx = filterCandidates allCandidates (CDigit("abdfg")) 

let testLine = parseLine "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

// printfn $"testline: {testLine}"

let filterAll (digits:List<CDigit>) =
    let acc = allCandidates
    digits |> List.fold filterCandidates acc 

let y = testLine.AllDigits |> filterAll 

// y |> Map.toList |> List.map (fun f -> printfn $"ENTRY: {f}")

// printfn $"y:{y}"

let removeCandidate (c:char) (li:char*Set<char>) = (fst li, snd li |> Set.remove c)

let rec permute (cand:List<char*Set<char>>) : list<list<char*char>> =
    let explode (segment:char*Set<char>):list<char*char> =
        let c = fst segment
        let cands = (snd segment) |> Set.toList
        cands |> List.map (fun v -> (c,v))
    if cand.Length = 1 then
        [explode cand.Head]
    else
        let heads = explode cand.Head
        let forHead (head:char*char) : list<list<char*char>> =
           let c = fst head
           let v = snd head
           let filteredTail = cand.Tail |> List.map (removeCandidate v)
           let permutedTail = permute filteredTail
           permutedTail |> List.map (fun tail -> head::tail)
        let perms = heads |> List.map forHead
        List.concat perms 
                           
let permutes = permute (y |> Map.toList) |> List.map Map  
// printfn $"{permutes}"

let toSegment (mapping:Map<char,char>) (digit:CDigit) =
    let actualDigits = digit.Wires |> Set.map (fun c -> mapping.TryFind c |> Option.defaultValue 'X')
    let segs = actualDigits |> Set.toSeq |> Seq.sort |> String.Concat 
    segs 

let toSegments (mapping:Map<char,char>) (digits:List<CDigit>) =
    digits |> List.map (toSegment mapping) 

let segs  =
    let digits = testLine.AllDigits
    // printfn $"digits: {digits}"
    // printfn $"permutes: {permutes}"
    let wires = permutes |> List.map (fun permute -> toSegments permute digits)
    // printfn $"wires: {wires}"
    wires

let toDec (segs:string) =
    match segs with
    | "ABCEFG" -> 0
    | "CF" -> 1
    | "ACDEG" -> 2
    | "ACDFG" -> 3
    | "BCDF" -> 4
    | "ABDFG" -> 5
    | "ABDEFG" -> 6
    | "ACF" -> 7
    | "ABCDEFG" -> 8
    | "ABCDFG" -> 9
    | _ -> -1

let valid (dec:list<int>) = not(dec |> List.contains -1)

let decs = segs |> List.map (List.map toDec)
let vdecs = decs |> List.filter valid 

// printfn $"segs: {segs}"
// printfn $"decs: {decs}"
// printfn $"vdecs: {vdecs}"

let isValid (line:CLine) (mapping:Map<char,char>) =
    let segs = line.AllDigits |> toSegments mapping
    let decs = segs |> List.map toDec
    not (decs |> List.contains -1) 

let findMappings (line:CLine) =
    let candidates = filterAll line.AllDigits
    let permutes:list<Map<char,char>> = permute (candidates |> Map.toList) |> List.map Map
    permutes |> List.filter (isValid line) 
    
let findMapping (line:CLine) =
    let maps = findMappings line
    if not (maps.Length = 1) then
        failwith $"found wrong number of valid mappings for line: ${line}"
    else
        maps.Head
        
let findValue (line:CLine) =
    let mapping = findMapping line
    let decs = toSegments mapping line.Answer |> List.map toDec 
    decs |> List.fold (fun acc v -> acc*10+v) 0 

// printfn $"value: ${findValue testLine}"

let sum = clines |> Array.map findValue |> Array.sum

printfn $"sum: {sum}"