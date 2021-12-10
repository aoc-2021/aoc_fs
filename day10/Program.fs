open System.IO
open System

let file = File.ReadAllLines "input.txt" |> Array.toList 
let input = file |> List.map (fun s -> s.ToCharArray () |> Array.toList)

printfn $"input {input}"

let valid = "{([][]())}".ToCharArray () |> Array.toList 
let invalid = "[([})}".ToCharArray () |> Array.toList
let incomplete = "{([][]()".ToCharArray () |> Array.toList

type Diag = Valid | Invalid | Incomplete

let map ending = [('(',')');('[',']');('{','}');('<','>')] |> Map 

let rec classifyNext(chars:list<char>) =
   printfn $"classify: {String.Concat (chars |> List.toArray)}"
   match chars with
   | [] -> Valid,[]
   | ['}'] -> Invalid,[]
   | ['>'] -> Invalid,[]
   | [')'] -> Invalid,[]
   | [']'] -> Invalid,[]
   | [_] -> Incomplete,[]
   | '('::')'::rest -> Valid,rest 
   | '<'::'>'::rest -> Valid,rest 
   | '['::']'::rest -> Valid,rest
   | '{'::'}'::rest -> Valid,rest
   | start::rest ->
       match classifyNext rest with
       | Valid,rest -> classifyNext (start::rest)
       | Invalid,_ -> Invalid,(start::rest)
       | Incomplete,_ -> Incomplete,(start::rest)
    
printfn $"{classifyNext valid}" 
printfn $"{classifyNext invalid}" 
printfn $"{classifyNext incomplete}" 
