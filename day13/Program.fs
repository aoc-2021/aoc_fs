open System
open System.IO

let input =
    File.ReadAllLines "input.txt" |> Array.toList

let rec getPoints (input: list<string>) =
    match input with
    | "" :: _ -> []
    | s :: tail ->
        let pairs = s.Split ','

        (pairs.[0] |> int, pairs.[1] |> int)
        :: getPoints tail

let rec getFolds (input: list<string>) =
    let getFold (s: string) =
        let fold = (s.Split ' ').[2].Split '='
        ((fold.[0].ToCharArray()).[0], fold.[1] |> int)

    input
    |> List.filter (fun s -> s.StartsWith "fold")
    |> List.map getFold

type Point = int * int
type Paper = List<Point>
let startPaper = getPoints input
let folds = getFolds input

let foldY (paper: Paper) (line: int) : Paper =
    let foldP ((x, y): Point) =
        if y < line then
            (x, y)
        else
            (x, line - (y - line))

    paper |> List.map foldP

let foldX (paper: Paper) (line: int) : Paper =
    let foldP ((x, y): Point) =
        if x < line then
            (x, y)
        else
            (line - (x - line), y)

    paper |> List.map foldP

let rmDupl (list: List<Point>) = list |> Set |> Set.toList

let fold (paper: Paper) ((axis, line): char * int) : Paper =
    let points =
        match axis with
        | 'x' -> foldX paper line
        | 'y' -> foldY paper line

    rmDupl points


let paper1 = fold startPaper (folds.Head)

printfn $"Task 1: {paper1.Length}"

let folded =
    folds
    |> List.fold (fun (paper: Paper) (f: char * int) -> fold paper f) startPaper

let paperPts = folded |> Set
let ys = { 1 .. 10 }
let xs = { 1 .. 80 }

let coords: List<int * int> =
    { 0 .. 10 }
    |> Seq.map (fun y -> { 0 .. 60 } |> Seq.map (fun x -> (x, y)))
    |> Seq.concat
    |> Seq.toList

printfn "Task 2:"

coords
|> List.map
    (fun point ->
        match point with
        | 60, _ -> printfn ""
        | p when paperPts.Contains p -> printf "█"
        | _ -> printf " ")
