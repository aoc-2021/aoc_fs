open System
open System.IO

let input1 = File.ReadAllLines "input.txt" |> Array.toList 

let input2 = ["6,10";
"0,14";
"9,10";
"0,3";
"10,4";
"4,11";
"6,0";
"6,12";
"4,1";
"0,13";
"10,12";
"3,4";
"3,0";
"8,4";
"1,10";
"2,14";
"8,10";
"9,0";
"";
"fold along y=7";
"fold along x=5"]

let input = input2

let rec getPoints(input:list<string>) =
    match input with
    | "" :: _ -> []
    | s :: tail ->
        let pairs = s.Split ','
        (pairs.[0] |> int, pairs.[1] |> int) :: getPoints tail
        
let rec getFolds (input:list<string>) =
    let getFold (s:string) =
        let fold = (s.Split ' ').[2].Split '='
        ((fold.[0].ToCharArray ()).[0],fold.[1] |> int)
    input |> List.filter (fun s -> s.StartsWith "fold") |> List.map getFold 
        
let startPaper = getPoints input
let folds = getFolds input

printfn $"startPaper {startPaper}"
printfn $"folds {folds}"