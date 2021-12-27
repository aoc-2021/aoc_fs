open System.IO

let _9to1 = [1L..9L] |> List.rev 
let file = File.ReadAllLines "input.txt"

type Register =
    | W 
    | X
    | Y
    | Z
    member this.Index =
        match this with
        | W -> 0
        | X -> 1
        | Y -> 2
        | Z -> 3
    static member ofString (s:string) : Register =
        match s with
        | "w" -> W
        | "x" -> X
        | "y" -> Y
        | "z" -> Z
        | "W" -> W
        | "X" -> X
        | "Y" -> Y
        | "Z" -> Z
    
type Value =
    | Reg of Register 
    | Literal of int64 

type Instruction =
    | Inp of Register
    | Add of Register*Value
    | Mul of Register*Value
    | Div of Register*Value
    | Mod of Register*Value
    | Eql of Register*Value

type Program = list<Instruction> 

type ALU (regs:Map<Register,int64>) =
    let get (value:Value) : int64 =
        match value with
        | Literal v -> v
        | Reg reg -> regs.TryFind reg |> Option.defaultValue 0L
    let set (reg:Register) (value:int64) =
        let regs = regs.Add (reg,value)
        ALU(regs)
    
    member this.Set = set 
        
    member this.Execute (i:Instruction) : Option<ALU> =
        match i with
        | Inp _ -> failwith "Inp is not supported"
        | Add (reg, value) ->
            let a = get (Reg reg)
            let b = get value 
            Some(set reg (a+b))
        | Mul (reg, value) ->
            let a = get (Reg reg)
            let b = get value  
            Some(set reg (a*b))
        | Div (reg, value) ->
            let a = get (Reg reg)
            let b = get value
            if b = 0 then
                printfn $"Divide by Zero {reg}/{value}"
                None
            else Some (set reg (a/b))
        | Mod (reg, value) ->
            let a = get (Reg reg)
            let b = get value
            if a < 0L || b <= 0L then
                printfn $"Mod fail {reg}={a} {value}={b}"
                None 
            else Some (set reg (a % b))
        | Eql (reg, value) ->
            let a = get (Reg reg)
            let b = get value
            Some (set reg (if a = b then 1L else 0L))
    static member empty = ALU(Map.empty)
    override this.ToString() =
        let w = regs.TryFind W |> Option.defaultValue 0L
        let x = regs.TryFind X |> Option.defaultValue 0L
        let y = regs.TryFind Y |> Option.defaultValue 0L
        let z = regs.TryFind Z |> Option.defaultValue 0L
        $"ALU(W={w} X={x} Y={y} Z={z})"

let parseLine (line:string) =
    let toReg = Register.ofString
    let isReg (s:string) = s = "w" || s = "W" || s = "x" || s = "X" || s = "y" || s = "Y" || s = "z" || s = "Z"
    let toVal (s:string) = if isReg s then Reg (toReg s) else Literal (s |> int64)
    let line = line.Split ' '
    match line with
    | [|"inp";reg|] -> Inp (toReg reg) 
    | [|"add";reg;value|] -> Add ((toReg reg),(toVal value))
    | [|"mul";reg;value|] -> Mul ((toReg reg),(toVal value))
    | [|"div";reg;value|] -> Div ((toReg reg),(toVal value))
    | [|"mod";reg;value|] -> Mod ((toReg reg),(toVal value))
    | [|"eql";reg;value|] -> Eql ((toReg reg),(toVal value))
    | _ -> failwith $"Cannot parse {line}"
 
let parseFile (input:string[]) : list<Instruction> =
    input |> Array.toList |> List.map parseLine 
        
let prodProgram = (parseFile file)

let rec exec (alu:ALU) (program:Program) : Option<list<int64>> =
    let execWithInput (alu:ALU) (reg:Register) (value:int64) (rest:Program) : Option<list<int64>> =
        let alu = alu.Set reg value
        let result = exec alu rest
        result |> Option.map (fun digits -> value::digits) 
    match program with
    | [] ->
        printfn $"ALU={alu}"
        Some([])
    | Inp reg :: rest ->
            _9to1
            |> List.toSeq
            |> Seq.map (fun n -> execWithInput alu reg n rest)
            |> Seq.tryFind Option.isSome
            |> Option.flatten 
    | inst::rest ->
        alu.Execute inst
        |> Option.map (fun alu -> exec alu rest)
        |> Option.flatten

let testProgram1 =
    [Inp X;Mul (X,Literal -1L)]

let testProgram2 =
    [Inp Z
     Inp X
     Mul (Z,Literal 3L)
     Eql (Z,Reg X)]

let testProgram3 =
    [Inp W
     Add (Z,(Reg W))
     Mod (Z, Literal 2L)
     Div (W,Literal 2L)
     Add (Y,Reg W)
     Mod (Y, Literal 2L)
     Div (W, Literal 2L)
     Add (X, Reg W)
     Mod (X, Literal 2L)
     Div (W, Literal 2L)
     Mod (W, Literal 2L)]

let myTestProgram4 =
    [Inp X
     Inp Y
     Inp Z 
     Mod (X, Literal 3L)
     Add (Y, Literal -1L)
     Mod (Z, Literal 3L)
     Div (Y, Reg X)
     Div (Y, Reg Z)]

let testProgram5 =
    [Add (X,Literal -5L)
     Add (Y,Literal 0L)
     Div (X,Reg Y)]
    

let result = exec ALU.empty prodProgram
printfn $"result = {result}"
        