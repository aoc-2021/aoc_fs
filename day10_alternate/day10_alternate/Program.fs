
open System.IO

let input = File.ReadAllLines "input.txt" |> Array.toList

let input2 = ["[({(<(())[]>[[{[]{<()<>>";
"[(()[<>])]({[<{<<[]>>(";
"{([(<{}[<>[]}>{[]{[(<()>";
"(((({<>}<{<{<>}{[]{[]{}";
"[[<[([]))<([[{}[[()]]]";
"[{[{({}]{}}([{[{{{}}([]";
"{<[[]]>}<{[{[{[]{()[[[]";
"[<(<(<(<{}))><([]([]()";
"<{([([[(<>()){}]>(<<{{";
"<{([{{}}[<[[[<>{}]]]>[]]"]

let removeAtoms (s:string) =
    let s = s.Replace ("()","")
    let s = s.Replace ("{}","")
    let s = s.Replace ("[]","")
    let s = s.Replace ("<>","")
    s
    
let rec removeComplete (s:string) =
    let lessAtoms = removeAtoms s
    if lessAtoms = s then s
    else removeComplete lessAtoms

let rests = input |> List.map removeComplete

let getClose (s:string) : Option<char> =
    s.ToCharArray () |> Array.tryFind (fun c -> c = ')' || c = '}' || c = '>' || c = ']')

let z = removeAtoms "xx{()}([])yy"
printfn $"{z}"
printfn $"rests={rests}"

let corrupt = rests |> List.map getClose |> List.filter Option.isSome
let incomplete = rests |> List.filter (fun s -> getClose s |> Option.isNone )

printfn $"corrupt={corrupt}"

let scoreOne (c:Option<char>) =
    match c with
    | Some(')') -> 3
    | Some(']') -> 57
    | Some('}') -> 1197
    | Some('>') -> 25137
    | _ -> failwith "invalid score char"

let score1 = corrupt |> List.map scoreOne |> List.sum
printfn $"task 1: {score1}"

// task 2

let toScore (c:char) =
    match c with
    | '(' -> 1L
    | '[' -> 2L
    | '{' -> 3L
    | '<' -> 4L
    | _ -> failwith $"invalid score2 char {c}"
    
let scoreLine (s:string) =
    let scores = s.ToCharArray () |> Array.toList |> List.map toScore |> List.rev
    scores |> List.fold (fun acc s -> acc*5L+s) 0L
    
let scoreLines = incomplete |> List.map scoreLine

printfn $"scoreLines {scoreLines}"
let score2 = scoreLines |> List.sort |> List.take ((scoreLines.Length+1)/2) |> List.rev |> List.head
printfn $"task2 : {score2}"