open System.IO

let file = File.ReadAllLines "input.txt" |> Array.toList 

let input2 = [
"NNCB";
"";
"CH -> B";
"HH -> N";
"CB -> H";
"NH -> C";
"HB -> C";
"HC -> B";
"HN -> C";
"NN -> C";
"BH -> H";
"NC -> B";
"NB -> B";
"BN -> B";
"BB -> N";
"BC -> B";
"CC -> N";
"CN -> C"]

let input = file

let template = input.Head.ToCharArray () |> Array.toList 

type Rule = (char*char)*char 
let rules : Map<char*char,char> =
    let toRule (s:string): Rule =
        let a = s.Split ' '
        let input = a.[0].ToCharArray () |> (fun a -> a.[0],a.[1])
        let output = (a.[2].ToCharArray ()).[0]
        (input,output) 
    input.Tail.Tail |> List.map toRule |> Map 
    
printfn $"template {template}"
printfn $"rules: {rules}"

let rec processTemplate (template:list<char>) (rules:Map<char*char,char>) =
   match template with
   | [] -> template 
   | [ _ ] -> template
   | a::b::tail ->
       match rules.TryFind (a,b) with
       | None -> a::processTemplate (b::tail) rules 
       | Some(c) -> a::c::processTemplate (b::tail) rules

let processed1 = processTemplate template rules

printfn $"processed: {processed1}"

let processed = {1..10} |> Seq.fold (fun acc _ -> processTemplate acc rules ) template

let elemCounts processed  = processed |> List.groupBy id |> List.map (fun (e,es) -> (e,es.Length |> int64))

let initElemCount = elemCounts template
let processedElemCount = elemCounts processed 

let minMax elemCounts = elemCounts |> List.fold (fun (small,big) (_,count) -> (min small count,max big count)) (1000000L,0L)

printfn $"minMax = {minMax (elemCounts processed)}"

let answer (small,big) = big - small

printfn $"Task1: {answer (minMax (elemCounts processed))}"

type Pair = char*char

let rec toPairs (l: list<char>) : List<Pair> =
    match l with
    | [] -> []
    | [_] -> []
    | a::b::tail -> (a,b)::toPairs(b::tail)

let initPairs = template |> toPairs |> List.groupBy id |> List.map (fun (pair,pairs) -> pair,pairs.Length |> int64)

printfn $"initElemCounts: {initElemCount}"
printfn $"initPairs: {initPairs}"

type ElemCounts = Map<char,int64>
type PairCounts = Map<Pair,int64>

let counts : ElemCounts = initElemCount |> Map

let addElements (elements:ElemCounts) (c:char) (count:int64) =
    printfn $"addelements {elements} {c} {count}"
    let count = elements.TryFind c |> Option.defaultValue 0 |> ((+) count)
    printfn $" count={count}"
    let e = elements.Add (c,count)
    printfn $"<- {e}"
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
        printfn $"processPair ({elements},{pairs} ({pair},{count})"
        match rules.TryFind pair with
        | None -> (elements,pairs)
        | Some (insertChar) ->
            let elements = addElements elements insertChar count
            let pairs = replacePair pairs pair insertChar count
            elements,pairs
    pairList |> List.fold processPair (elements,pairs)   
            
let fproc1 = fastProcess (counts,initPairs |> Map)

printfn $"fproc1 {fproc1}"

let fprocessed = {1..40} |> Seq.fold (fun acc _ -> fastProcess acc) (counts,initPairs |> Map) 

printfn $"fproc10 = {fprocessed}"

let fastCounts = fprocessed |> fst |> Map.toList |> List.map snd
let fastMax = fastCounts |> List.max
let fastMin = fastCounts |> List.min

printfn $"Task2 : {fastMax-fastMin} {fastMin} {fastMax}"