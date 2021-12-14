open System.IO

let input = File.ReadAllLines "input.txt" |> Array.toList 

let template = input.Head.ToCharArray () |> Array.toList 

type Rule = (char*char)*char 
let rules : Map<char*char,char> =
    let toRule (s:string): Rule =
        let a = s.Split ' '
        let input = a.[0].ToCharArray () |> (fun a -> a.[0],a.[1])
        let output = (a.[2].ToCharArray ()).[0]
        (input,output) 
    input.Tail.Tail |> List.map toRule |> Map 
    
let elemCounts processed  = processed |> List.groupBy id |> List.map (fun (e,es) -> (e,es.Length |> int64))

let initElemCount = elemCounts template

type Pair = char*char

let rec toPairs (l: list<char>) : List<Pair> =
    match l with
    | [] -> []
    | [_] -> []
    | a::b::tail -> (a,b)::toPairs(b::tail)

let initPairs = template |> toPairs |> List.groupBy id |> List.map (fun (pair,pairs) -> pair,pairs.Length |> int64)

type ElemCounts = Map<char,int64>
type PairCounts = Map<Pair,int64>

let counts : ElemCounts = initElemCount |> Map

let addElements (elements:ElemCounts) (c:char) (count:int64) =
    let count = elements.TryFind c |> Option.defaultValue 0 |> ((+) count)
    let e = elements.Add (c,count)
    e 

let addToPair (pairs:PairCounts) (pair:Pair) (count:int64) =
    match pairs.TryFind pair with
    | None -> pairs.Add (pair,count)
    | Some(prevCount) -> pairs.Add(pair,prevCount+count)

let replacePair (pairs:PairCounts) (pair:Pair) (c:char) (count:int64) =
    let prevCount : int64 = pairs.TryFind pair |> Option.get 
    let pairs = pairs.Add(pair,prevCount-count)
    let pair1 = (fst pair,c)
    let pair2 = (c,snd pair)
    let pairs = addToPair pairs pair1 count
    let pairs = addToPair pairs pair2 count
    pairs

let rec fastProcess ((elements,pairs):ElemCounts*PairCounts) : ElemCounts*PairCounts =
    let pairList = pairs |> Map.toList
    let processPair ((elements,pairs):ElemCounts*PairCounts) ((pair,count):Pair*int64) =
        match rules.TryFind pair with
        | None -> (elements,pairs)
        | Some (insertChar) ->
            let elements = addElements elements insertChar count
            let pairs = replacePair pairs pair insertChar count
            elements,pairs
    pairList |> List.fold processPair (elements,pairs)   
            
let fproc1 = fastProcess (counts,initPairs |> Map)


let fprocess n = {1..n} |> Seq.fold (fun acc _ -> fastProcess acc) (counts,initPairs |> Map)


let fastCounts fresult = fresult |> fst |> Map.toList |> List.map snd

let solve (n:int) =
    let fprocessed = fprocess n
    let fastCounts = fastCounts fprocessed
    let fastMax = fastCounts |> List.max
    let fastMin = fastCounts |> List.min
    fastMax - fastMin 

let stopWatch1 = System.Diagnostics.Stopwatch.StartNew()
printfn $"Task 1: {solve 10}"
stopWatch1.Stop ()
printfn $"{stopWatch1.Elapsed.TotalMilliseconds}"

let stopWatch2 = System.Diagnostics.Stopwatch.StartNew()
printfn $"Task 2: {solve 40}"
stopWatch2.Stop ()
printfn $"{stopWatch2.Elapsed.TotalMilliseconds}"

// Task 1: 4517
// Task 2: 4704817645083