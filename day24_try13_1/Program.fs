open System.IO

let file = File.ReadAllLines "input.txt"

type Reg =
    | W
    | X
    | Y
    | Z

type Param =
    | R of Reg
    | I of int64 
type Inst =
    | INP of int*Reg
    | ADD of Reg*Param
    | MUL of Reg*Param
    | DIV of Reg*int64
    | MOD of Reg*int64
    | EQL of Reg*Param
    | SET of Reg*int64
    | NOP 

let readProgram (file:string) =
    let input = File.ReadAllLines file
                |> Array.toList
                |> List.map (fun s -> s.Split " ")
    let parseLine (line:string[]) (inputNo:int) : int*Inst=
        let toReg (s:string) =
            match s with
            | "w" -> W
            | "x" -> X
            | "y" -> Y
            | "z" -> Z 
        let toValue (s:string) =
            match s with
            | "w" -> R W
            | "x" -> R X
            | "y" -> R Y
            | "z" -> R Z
            | _ -> int64 s |> I 
        match line with
        | [|"add";_;"0"|]     -> inputNo,NOP 
        | [|"add";reg;value|] -> inputNo,ADD (toReg reg,toValue value)
        | [|"mul";reg;"0"|]   -> inputNo,SET ((toReg reg),0L)
        | [|"mul";_;"1"|]   -> inputNo,NOP
        | [|"mul";reg;value|] -> inputNo,MUL (toReg reg,toValue value)
        | [|"div";_;"1"|] -> inputNo,NOP
        | [|"div";reg;value|] -> inputNo,DIV (toReg reg,int64 value)
        | [|"mod";reg;value|] -> inputNo,MOD (toReg reg,int64 value)
        | [|"eql";reg;value|] -> inputNo,EQL (toReg reg,toValue value)
        | [|"inp";reg|]       -> inputNo+1,INP (inputNo,toReg reg)
    let rec parse (lines:list<string[]>) (inputNo:int) =
        printfn $"parse {lines} {inputNo}" 
        match lines with
        | [] -> []
        | line::rest ->
            let inputNo,inst = parseLine line inputNo 
            inst::(parse rest inputNo) 
    parse input 0    
     
readProgram "input.txt" |> List.map (printfn "%A")

type Sources(inputs:Map<int,Set<int64>>) =
    member this.Inputs = inputs
    override this.ToString() = "TODO"

type Number (value:int64,sources:Sources)  =
    interface System.IComparable with
        override this.CompareTo(other) =
            match other with 
            | :? Number as other ->
                if value > other.Value then 1
                else if value < other.Value then -1
                else 0
            | _ -> 1
    override this.ToString () = $"'{value}"
                
    member this.Value = value 
    member this.Sources = sources

type Value(vals:Set<Number>) =
    member this.Vals = vals 

    static member Input (id:int) = 
        let input (i:int) = Number(i|>int64,Sources([(id,[i|>int64]|>Set)] |> Map))
        [1..9] |> List.map input  |> Set |> Value
    
    override this.ToString () =
        vals |> Set.map (sprintf "%A") |> String.concat " "

Value.Input 1 |> (printfn "%A")
    