open System.IO

let file = File.ReadAllLines "input.txt" |> Array.toList
let testFile = File.ReadAllLines "testInput.txt" |> Array.toList 

type Pos = int64*int64
type Digit = int64 

type Frame(x1:int64,y1:int64,x2:int64,y2:int64) =
    member this.X1 = x1
    member this.Y1 = y1
    member this.X2 = x2
    member this.Y2 = y2

type Enhancer(digits:Digit[]) =
    member this.Digits = digits 
type Image(image:Map<Pos,int64>,frame:Frame) =
    member this.Image = image
    member this.Frame = frame 

let charToDigit (c:char) =
    match c with
    | '.' -> 0L
    | '#' -> 1L
    | _ -> failwith $"Unknown digit char: {c}"

let readInput (input:list<string>) =
    let enhancer = Enhancer(input.Head.ToCharArray() |> Array.map charToDigit)
    let imageArray = input.Tail.Tail |> List.toArray
    let imageArray = imageArray |> Array.map (fun line -> line.ToCharArray () |> Array.map charToDigit)
    let maxX = imageArray.[0].Length - 1 |> int64
    let maxY = imageArray.Length - 1 |> int64 
    let allPositions = Seq.allPairs {0L..maxX} {0L..maxY} |> Seq.toList
    let allPixels = allPositions
                    |> List.map (fun (x,y) -> ((x,y),imageArray.[y |> int].[x |> int]))
                    |> Map
    let frame = Frame(-1L,-1L,maxX+1L,maxY+1L)
    let image = Image(allPixels,frame)
    enhancer,image 
    
let input = testFile

let enhancer,image = readInput input

printfn $"enhancer: {enhancer.Digits.Length}"