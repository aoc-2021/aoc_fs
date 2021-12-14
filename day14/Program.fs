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
    
let rec processTemplate (template:list<char>) (rules:Map<char*char,char>) =
   match template with
   | [] -> template 
   | [ _ ] -> template
   | a::b::tail ->
       match rules.TryFind (a,b) with
       | None -> a::processTemplate (b::tail) rules 
       | Some(c) -> a::c::processTemplate (b::tail) rules

let processed1 = processTemplate template rules

let processed = {1..10} |> Seq.fold (fun acc _ -> processTemplate acc rules ) template

let elemCounts processed  = processed |> List.groupBy id |> List.map (fun (e,es) -> (e,es.Length |> int64))

let initElemCount = elemCounts template
let processedElemCount = elemCounts processed 

let minMax elemCounts = elemCounts |> List.fold (fun (small,big) (_,count) -> (min small count,max big count)) (1000000L,0L)

let answer (small,big) = big - small

printfn $"Task 1: {answer (minMax (elemCounts processed))}"

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


let fprocessed = {1..40} |> Seq.fold (fun acc _ -> fastProcess acc) (counts,initPairs |> Map) 

let fastCounts = fprocessed |> fst |> Map.toList |> List.map snd
let fastMax = fastCounts |> List.max
let fastMin = fastCounts |> List.min

printfn $"Task 2: {fastMax-fastMin}"