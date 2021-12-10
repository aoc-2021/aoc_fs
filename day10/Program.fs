open System.IO
open System

let file = File.ReadAllLines "input.txt" |> Array.toList

let fileX = [
    "[({(<(())[]>[[{[]{<()<>>";
"[(()[<>])]({[<{<<[]>>(";
"{([(<{}[<>[]}>{[]{[(<()>";
"(((({<>}<{<{<>}{[]{[]{}";
"[[<[([]))<([[{}[[()]]]";
"[{[{({}]{}}([{[{{{}}([]";
"{<[[]]>}<{[{[{[]{()[[[]";
"[<(<(<(<{}))><([]([]()";
"<{([([[(<>()){}]>(<<{{";
"<{([{{}}[<[[[<>{}]]]>[]]"]

let input = file |> List.map (fun s -> s.ToCharArray () |> Array.toList)

printfn $"input {input}"

let valid = "{([][]())}".ToCharArray () |> Array.toList 
let invalid = "[([})}".ToCharArray () |> Array.toList
let incomplete = "{([][]()".ToCharArray () |> Array.toList

type Diag = Valid | Invalid | Incomplete

let map ending = [('(',')');('[',']');('{','}');('<','>')] |> Map 

let rec classifyNext(chars:list<char>) =
//   printfn $"classify: {String.Concat (chars |> List.toArray)}"
   match chars with
   | [] -> Valid,' ',[]
   | '}'::_ -> Invalid,'}',[]
   | '>'::_ -> Invalid,'>',[]
   | ')'::_ -> Invalid,')',[]
   | ']'::_ -> Invalid,']',[]
   | [c] -> Incomplete,' ',[c]
   | '('::')'::rest -> Valid,' ',rest 
   | '<'::'>'::rest -> Valid,' ',rest 
   | '['::']'::rest -> Valid,' ',rest
   | '{'::'}'::rest -> Valid,' ',rest
   | start ::'}'::rest -> Invalid,'}',rest
   | start ::'>'::rest -> Invalid,'>',rest
   | start ::']'::rest -> Invalid,']',rest
   | start ::')'::rest -> Invalid,')',rest
   | start::rest ->
       match classifyNext rest with
       | Valid,_,rest -> classifyNext (start::rest)
       | Invalid,c,_ -> Invalid,c,(start::rest)
       | Incomplete,c,rest -> Incomplete,c,(start::rest)
    
printfn $"{classifyNext valid}" 
printfn $"{classifyNext invalid}" 
printfn $"{classifyNext incomplete}" 

let corrupted = input |> List.map classifyNext |> List.filter (fun (stat,_,_) -> stat = Invalid)

printfn $"{corrupted.Length} {corrupted}"

let illegalchars = corrupted |> List.map (fun (_,c,_) -> c)
printfn $"{illegalchars}"

let scoreChar (c:char) =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | c -> failwith $"Invalid invalid char: {c}"

let score = illegalchars |> List.map scoreChar |> List.sum

printfn $"score: {score}"

// part 2

let isIncomplete line =
    let (c,_,_) = classifyNext line
    c <> Invalid 

let incompleteLines = input |> List.filter isIncomplete

let rests = incompleteLines |> List.map classifyNext |> List.map (fun (_,_,r) -> r)


let input1 = "[({(<(())[]>[[{[]{<()<>>".ToCharArray () |> Array.toList |> List.rev


let rec rmNext (chars:list<char>) =
    match chars with
    | ')'::'('::tail -> tail
    | '}'::'{'::tail -> tail
    | ']'::'['::tail -> tail
    | '>'::'<'::tail -> tail
    | last :: front -> rmNext (last::(rmNext front))
    
let rec complete (chars:list<char>) =
    match chars with
    | [] -> [] 
    | '{'::tail -> '}' :: (complete tail)
    | '['::tail -> ']' :: (complete tail)
    | '<'::tail -> '>' :: (complete tail)
    | '('::tail -> ')' :: (complete tail)
    | _ -> complete (rmNext chars)
    
let z = complete ("([{}{}[()]".ToCharArray() |> Array.toList |> List.rev )
printfn $"z={z}"

let x = complete input1
printfn $"x = {x}"

let completes = incompleteLines |> List.map List.rev |> List.map complete

printfn $"completes = {completes}"

let scoreChars (chars:list<char>) =
    let scoreChar (c:char) : int64 =
        match c with
        | ')' -> 1L
        | ']' -> 2L
        | '}' -> 3L
        | '>' -> 4L
        | _ -> failwith $"Invalid score char {c}"
    chars |> List.fold (fun (acc:int64) (c:char) -> acc*5L+(scoreChar c)) 0L

let final = scoreChars [']';')';'}';'>']
printfn $"final: {final}"

let scores = completes |> List.map scoreChars

let middle = scores |> List.sort |> List.take ((scores.Length+1)/2) |> List.rev |> List.head  

printfn $"scores {scores} middle={middle}"