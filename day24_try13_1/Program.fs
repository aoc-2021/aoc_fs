open System.IO

let file = File.ReadAllLines "input.txt"

type Reg =
    | W
    | X
    | Y
    | Z

type Sources(inputs:List<Map<int,Set<int64>>>) =
    member this.Inputs = inputs
    override this.ToString() =
        let inputsToString (is:Set<int64>) = is |> Set.toList |> List.map string |> String.concat ""
        let pairToString (k,vs) = $"{k}:{vs|>inputsToString}"
        inputs |> List.map (fun m -> m |> Map.toList |> List.map pairToString |> String.concat ",") |> String.concat ";"

type SourcedNumber (value:int64,sources:Sources)  =
    interface System.IComparable with
        override this.CompareTo(other) =
            match other with 
            | :? SourcedNumber as other ->
                if value > other.Value then 1
                else if value < other.Value then -1
                else 0
            | _ -> 1
    override this.ToString () = $"'{value}[{sources}]"
                
    member this.Value = value 
    member this.Sources = sources

type SourcedValue(vals:Set<SourcedNumber>) =
    member this.Vals = vals 

    static member Input (id:int) = 
        let input (i:int) = SourcedNumber(i|>int64,Sources([(id,[i|>int64]|>Set)] |> Map |> List.singleton))
        [1..9] |> List.map input  |> Set |> SourcedValue
    
    override this.ToString () =
        vals |> Set.map (sprintf "%A") |> String.concat " "

type Value =
    | UNKNOWN
    | CONST of int64
    | FROM of int64
    | TO of int64
    | VALUES of SourcedValue
    | REG of Reg

let value2String (value:Value) =
    match value with
    | UNKNOWN -> "?"
    | CONST i -> i |> string
    | FROM i -> $"{i}..."
    | TO i -> $"...{i}"
    | VALUES v -> v |> string
    | REG r -> r |> string 

type Param =
    | R of Reg
    | I of int64

let param2String (param:Param) =
    match param with
    | R r -> $"{r}"
    | I i -> $"{i}"
type Inst =
    | INP of Reg*Value
    | ADD of Reg*Value
    | MUL of Reg*Value
    | DIV of Reg*int64
    | MOD of Reg*int64
    | EQL of Reg*Value 
    | SET of Reg*int64
    | NOP

let inst2String (inst:Inst) =
    match inst with
    | INP (r,value) -> $"{r} ⭊ {value |> value2String}"
    | ADD (r,value) -> $"{r} := {r} + {value |> value2String}"
    | MUL (r,value) -> $"{r} := {r} * {value |> value2String}"
    | DIV (r,value) -> $"{r} := {r} / {value}"
    | MOD (r,value) -> $"{r} := {r} %% {value}"
    | EQL (r,value) -> $"{r} := {r} = {value |> value2String}"
    | SET (r,value) -> $"{r} := {value}"
    | NOP -> "...        "
    | _ -> inst |> string 

type ALU (regs:Map<Reg,Value>) =
    member this.Get (reg:Reg)  = regs.TryFind reg |> Option.get
    member this.Set (reg:Reg) (value:Value) = regs.Add (reg,value) |> ALU 
    override this.ToString() =
        ALU.allRegs
        |> List.filter (fun r -> this.Get r <> UNKNOWN)
        |> List.map (fun r -> r,this.Get r)
        |> List.map (fun (r,value) -> $"{r}:{value |> value2String}" ) |> String.concat ","
    static member allRegs = [W;X;Y;Z]
    static member allRegsExcept (reg:Reg) = ALU.allRegs |> List.filter ((<>) reg)
    static member initial = ALU.allRegs |> List.map (fun i -> i,CONST 0L) |> Map |> ALU
    static member unknown = ALU.allRegs |> List.map (fun i -> i,UNKNOWN) |> Map |> ALU 

let readInstructions (file:string) =
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
            | "w" -> REG W
            | "x" -> REG X
            | "y" -> REG Y
            | "z" -> REG Z
            | _ -> int64 s |> CONST 
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
        | [|"inp";reg|]       -> inputNo+1,INP ((toReg reg),VALUES (SourcedValue.Input inputNo))
    let rec parse (lines:list<string[]>) (inputNo:int) =
        match lines with
        | [] -> []
        | line::rest ->
            let inputNo,inst = parseLine line inputNo 
            inst::(parse rest inputNo) 
    parse input 0    
     

type Step (inst:Inst, input:ALU, output:ALU) =
    member this.Inst = inst
    member this.Input = input
    member this.Output = output
    
    override this.ToString () =
        $"⍗ {inst |> inst2String,-15} IN:{input,30}   OUT:{output,40}"
    
let instructionsToProgram (instructions:list<Inst>) =
    let rec convert (instructions:list<Inst>) =
        match instructions with
        | [] -> []
        | [inst] -> [Step(inst,ALU.unknown,ALU.unknown.Set Z (CONST 0L))]
        | inst::rest -> Step(inst,ALU.unknown,ALU.unknown) :: convert(rest)
    let program = convert instructions
    let first = program.Head
    let first = Step(first.Inst,ALU.initial,first.Output)
    first :: program.Tail

let readInput (filename:string) : List<Step> =
    readInstructions filename
    |> instructionsToProgram
        
let program = readInput "input.txt"

program |> List.map (printfn "%A")            

    