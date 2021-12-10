open System.IO

let input =
    File.ReadAllLines "input.txt" |> Array.toList

let lines =
    input
    |> List.map (fun s -> s.ToCharArray() |> Array.toList)

type Stack = list<char>

type ParseResult =
    | Corrupt of char
    | Incomplete of list<char>
    | Valid of Stack

let close (stack: Stack) (c: char) =
    match stack, c with
    | '{' :: stack, '}' -> Valid stack
    | '[' :: stack, ']' -> Valid stack
    | '(' :: stack, ')' -> Valid stack
    | '<' :: stack, '>' -> Valid stack
    | _, c -> Corrupt c

let rec parse (stack: Stack) (line: list<char>) =
    let closing c =
        c = '>' || c = '}' || c = ']' || c = ')'

    match line with
    | [] ->
        if stack.IsEmpty then
            Valid []
        else
            Incomplete stack
    | c :: rest when closing c ->
        match close stack c with
        | Valid stack -> parse stack rest
        | Corrupt c -> Corrupt c
        | _ -> failwith "Impossible!"
    | c :: rest -> parse (c :: stack) rest

let results = lines |> List.map (parse [])

let corruptScore (p: ParseResult) =
    match p with
    | Corrupt ')' -> 3L
    | Corrupt ']' -> 57L
    | Corrupt '}' -> 1197L
    | Corrupt '>' -> 25137L
    | _ -> 0L

let incompleteScore (p: ParseResult) =
    let charScore c =
        match c with
        | '(' -> 1L
        | '[' -> 2L
        | '{' -> 3L
        | '<' -> 4L
        | _ -> failwith "Impossible!"

    let addToScore (score: int64) (c: char) = (score * 5L) + (charScore c)

    match p with
    | Corrupt _ -> 0L
    | Incomplete stack -> stack |> List.fold addToScore 0L
    | Valid _ -> failwith "Impossible!"

let median (l: list<int64>) =
    l
    |> List.sort
    |> List.take ((l.Length + 1) / 2)
    |> List.last

let task1 =
    results |> List.map corruptScore |> List.sum

let task2 =
    results
    |> List.map incompleteScore
    |> List.filter ((<) 0)
    |> median

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
