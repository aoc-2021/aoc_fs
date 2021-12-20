open System.IO

let file =
    File.ReadAllLines "input.txt" |> Array.toList

let testFile =
    File.ReadAllLines "testInput.txt" |> Array.toList

type Pos = int64 * int64
type Digit = int64

type Frame(x1: int64, y1: int64, x2: int64, y2: int64) =
    member this.X1 = x1
    member this.Y1 = y1
    member this.X2 = x2
    member this.Y2 = y2

    member this.AllPositions() : list<Pos> =
        Seq.allPairs { x1 .. x2 } { y1 .. y2 }
        |> Seq.toList
    member this.Expand () : Frame =
        Frame(x1-2L,y1-2L,x2+2L,y2+2L)
    
    override this.ToString () =
        $"Frame{(x1,y1,x2,y2)}"

type Enhancer(digits: Digit []) =
    member this.Digits = digits
    member this.GetPixel(index: int64) = digits.[index |> int]

type Image(image: Map<Pos, int64>, frame: Frame, def:Digit) =
    member this.Image = image
    member this.Frame = frame

    member this.getPixel(pos: Pos) : Digit =
        image.TryFind pos |> Option.defaultValue def 

    member this.getAreaPixels((x, y): Pos) : list<Digit> =
        let pixels =
            [ (x - 1L, y - 1L)
              (x     , y - 1L)
              (x + 1L, y - 1L)
              (x - 1L, y)
              (x     , y)
              (x + 1L, y)
              (x - 1L, y + 1L)
              (x     , y + 1L)
              (x + 1L, y + 1L) ]

        pixels |> List.map this.getPixel
    member this.Def = def 
        
    override This.ToString () =
        $"Image({image} {frame})"
        
// let blankImage = Image(Map.empty, Frame(0L, 0L, 0L, 0L))

let binaryToInt (bin: list<Digit>) : int64 =
    let rec addBinaryDigit (acc: int64) (bin: list<Digit>) =
        match bin with
        | [] -> acc
        | d :: rest -> addBinaryDigit (acc * 2L + d) rest

    addBinaryDigit 0L bin

let charToDigit (c: char) =
    match c with
    | '.' -> 0L
    | '#' -> 1L
    | _ -> failwith $"Unknown digit char: {c}"

let readInput (input: list<string>) =
    let enhancer =
        Enhancer(input.Head.ToCharArray() |> Array.map charToDigit)

    let imageArray = input.Tail.Tail |> List.toArray

    let imageArray =
        imageArray
        |> Array.map (fun line -> line.ToCharArray() |> Array.map charToDigit)

    let maxX = imageArray.[0].Length - 1 |> int64
    let maxY = imageArray.Length - 1 |> int64

    let allPositions =
        Seq.allPairs { 0L .. maxX } { 0L .. maxY }
        |> Seq.toList

    let allPixels =
        allPositions
        |> List.map (fun (x, y) -> ((x, y), imageArray.[y |> int].[x |> int]))
        |> Map

    let frame = Frame(-2L, -2L, maxX + 2L, maxY + 2L)
    let image = Image(allPixels, frame,0L)
    enhancer, image

let input = file

let enhancer, image = readInput input

printfn $"enhancer: {enhancer.Digits.Length}"

let enhance (enhancer: Enhancer) (image: Image) : Image =
    let positions = image.Frame.AllPositions()

    let nextMap = positions
                  |> List.map (fun pos -> pos, image.getAreaPixels pos)
                  |> List.map (fun (pos, binary) -> (pos, binaryToInt binary))
                  |> List.map (fun (pos,index) -> (pos,enhancer.GetPixel index))
                  |> Map
    let frame = image.Frame.Expand ()
    let def = image.getAreaPixels (10000000000L,100000000L) |> binaryToInt |> int64 |> enhancer.GetPixel
    // printfn $"Default: {def}"
    Image(nextMap,frame,def)
    
let image1 = enhance enhancer image
let image2 = enhance enhancer image1

let printImage (image:Image) =
    let toPrintable (pixel:Digit) = if pixel = 1L then '#' else '.'
    printfn $"Printing image {image}"
    let ys = {image.Frame.Y1..image.Frame.Y2} |> Seq.toList 
    let xs = {image.Frame.X1..image.Frame.X2} |> Seq.toList 
    ys |> List.map (fun y ->
            xs |> List.map (fun x -> printf $"{image.getPixel(x,y) |> toPrintable}")
            printfn ""                
        )
    printfn ""  

let countLit (image:Image) : int =
    image.Image.Values |> Seq.filter ((=) 1L) |> Seq.length

printImage image 
printImage image1
printImage image2

printfn $"image@2: {image2} : lit = {countLit image2}"

printfn $"image:(1,2) = {image.getPixel (1L,2L)}"

printfn $"{image.getAreaPixels (2L,2L) |> binaryToInt}"

let image50 = {0..49} |> Seq.toList |> List.fold (fun image _ -> enhance enhancer image) image 

printfn $"image50 {countLit image50}"