open System
open System.IO

let input =
    File.ReadAllLines "input.txt" |> Array.toList

type Cave = int
type Tube = Cave * Cave

let stringTubes = input |> List.map (fun s -> s.Split '-')

let caveValueMap =
    let names =
        stringTubes
        |> List.map Array.toList
        |> List.concat
        |> Set
        |> Set.toArray

    let isUpperCase (s: string) =
        s.ToCharArray() |> Array.exists Char.IsUpper

    names
    |> Array.mapi (fun i v -> (v, i * if isUpperCase v then -1 else 1))
    |> Map

printfn $"caveValueMap {caveValueMap}"

let allTubesForward: Set<Tube> =
    let toCave (s: string) = caveValueMap.TryFind s |> Option.get

    stringTubes
    |> List.map (fun s -> (s.[0] |> toCave, s.[1] |> toCave))
    |> Set

let START =
    caveValueMap.TryFind "start" |> Option.get

let END = caveValueMap.TryFind "end" |> Option.get

let allTubesBackward: Set<Tube> =
    allTubesForward |> Set.map (fun (a, b) -> (b, a))

let allTubes =
    Set.union allTubesForward allTubesBackward

let starts =
    allTubes |> Set.filter (fun (b, _) -> b = START)

let tubes =
    allTubes
    |> Set.filter (fun (a, b) -> a = START || b = START |> not)

type CaveMap = Map<Cave, list<Cave>>

let caveMap: CaveMap =
    tubes
    |> Set.toList
    |> List.groupBy fst
    |> List.map (fun (cave, tubes) -> (cave, (tubes |> List.map snd)))
    |> Map

let isSmall (cave: Cave) = cave > -1

type Visited = Set<int> 
type Memo = Map<bool*Visited*Cave,int>

let rec expand (memo:Memo) (denySecondVisits: bool) (visitedSmall: Visited) (caveMap: CaveMap) (e: Cave) : Memo*int =
   let memoKey = (denySecondVisits,visitedSmall,e)
   let memoValue = memo.TryFind(memoKey)
   if memoValue.IsSome then memo,memoValue |> Option.get
   else 
        let secondVisit = isSmall e && visitedSmall.Contains e
        let visitedSmall = if isSmall e then visitedSmall.Add e else visitedSmall
        if secondVisit && denySecondVisits then memo,0
        elif e = END then memo,1
        else
            let denySecondVisits = denySecondVisits || secondVisit
            let nexts = caveMap.TryFind e |> Option.defaultValue []
        
            let (memo,count): Memo*int =
                let callNext ((memo,count):Memo*int) (cave:Cave) =
                    let memo,more = expand memo denySecondVisits visitedSmall caveMap cave
                    memo,(count+more)
                nexts |> List.fold callNext (memo,0) 

            let memo = memo.Add (memoKey,count)
            let memoOk = match memoValue with
                         | Some(memoed) -> memoed = count
                         | None -> true
            if not memoOk then printfn $"memoed wrong: {memoValue} actual={count}"
            (memo,count)

let paths1 =
    starts
    |> Set.toList
    |> List.map snd
    |> List.map (expand Map.empty true Set.empty caveMap)
    |> List.map snd 
    |> List.sum 

let paths2 =
    starts
    |> Set.toList
    |> List.map snd
    |> List.map (expand Map.empty false Set.empty caveMap)
    |> List.map snd 
    |> List.sum 

printfn $"Task 1: {paths1}" // correct is 3463
printfn $"Task 2: {paths2}" // correct is 91533
