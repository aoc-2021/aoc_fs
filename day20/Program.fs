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
        Frame(x1-1L,y1-1L,x2+1L,y2+1L)
    
    override this.ToString () =
        $"Frame{(x1,y1,x2,y2)}"

type Enhancer(digits: Digit []) =
    member this.Digits = digits
    member this.GetPixel(index: int64) = digits.[index |> int]

type Image(image: Map<Pos, int64>, frame: Frame) =
    member this.Image = image
    member this.Frame = frame

    member this.getPixel(pos: Pos) : Digit =
        image.TryFind pos |> Option.defaultValue 0L

    member this.getAreaPixels((x, y): Pos) : list<Digit> =
        let pixels =
            [ (x - 1L, y - 1L)
              (x, y - 1L)
              (x, y + 1L)
              (x - 1L, y)
              (x, y)
              (x + 1L, y)
              (x - 1L, y + 1L)
              (x, y + 1L)
              (x + 1L, y + 1L) ]

        pixels |> List.map this.getPixel
        
    override This.ToString () =
        $"Image({image} {frame})"
        
let blankImage = Image(Map.empty, Frame(0L, 0L, 0L, 0L))

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

    let frame = Frame(-1L, -1L, maxX + 1L, maxY + 1L)
    let image = Image(allPixels, frame)
    enhancer, image

let input = testFile

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
    Image(nextMap,frame)
    
let image1 = enhance enhancer image
let image2 = enhance enhancer image1

let countLit (image:Image) : int =
    image.Image.Values |> Seq.filter ((=) 1L) |> Seq.length

printfn $"image@1: {image2} : lit = {countLit image2}"