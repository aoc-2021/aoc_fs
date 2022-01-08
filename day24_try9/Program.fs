open System.IO

let file =
    File.ReadAllLines "input.txt" |> Array.toList

type Reg =
    | W
    | X
    | Y
    | Z

let ALL_REGS = [ W; X; Y; Z ]
let otherRegs (reg: Reg) = ALL_REGS |> List.filter ((<>) reg)


type Param =
    | R of Reg
    | I of int64
    | NA

let paramToString (param:Param) =
    match param with
    | R reg -> $"{reg} "
    | I i -> $"{i}"
    | NA -> ""

type Instruction =
    | I_INP of Reg 
    | I_ADD of Reg*Param 
    | I_MUL of Reg*Param 
    | I_DIV of Reg*int64 
    | I_MOD of Reg*int64 
    | I_EQL of Reg*Param 
type Program = list<Instruction> 

let printProgram (program:Program) =
    let nums = [1..program.Length]
    let nInsts = List.zip nums program
    nInsts |> List.map (fun (i,inst) -> printfn $"{i,3} {inst}")

let parseLine (line: string) : Instruction =
    let line = line.Split ' '

    let toReg (s: string) : Reg =
        match s with
        | "w" -> W
        | "x" -> X
        | "y" -> Y
        | "z" -> Z
        | _ -> failwith $"not a register: {s}"

    let toParam (s: string) =
        match s with
        | "w" -> R W
        | "x" -> R X
        | "y" -> R Y
        | "z" -> R Z
        | _ -> s |> int64 |> I

    match line with
    | [| "add"; r; p |] -> I_ADD (r |> toReg,p |> toParam)
    | [| "mul"; r; p |] -> I_MUL (r |> toReg,p |> toParam)
    | [| "div"; r; p |] -> I_DIV (r |> toReg,p |> int64)
    | [| "mod"; r; p |] -> I_MOD (r |> toReg,p |> int64)
    | [| "eql"; r; p |] -> I_EQL (r |> toReg,p |> toParam)
    | [| "inp"; r |] -> I_INP (r|> toReg)

let program = file |> List.map parseLine


type Value =
    | INPUT of int
    | SUM of Value*Value
    | MUL of Value
    | MOD of Value
    | DIV of Value
    | CONST of int64 
    
type ALU(regs:Map<Reg,Value>,lastInput:int) =
    member this.Regs = regs
    member this.get (reg:Reg) = regs.TryFind reg |>Option.get
    member this.set (reg:Reg) (value:Value) =
        let regs = regs.Add(reg,value)
        ALU(regs,lastInput)
        
    member this.exec (inst:Instruction) : ALU =
        match inst with
        | I_INP reg ->
            let lastInput = lastInput + 1 
            let regs = regs.Add(reg,(INPUT lastInput))
            ALU(regs,lastInput)
        | _ ->
            printfn $"Not implemented : {inst}"
            this 

    override this.ToString() =
        let rs = [W;X;Y;Z] |> List.map (fun r -> r,this.get r) |> List.map (fun (r,v) -> sprintf $"{r}={v}") |> String.concat " "
        $"ALU {rs}"
    
    static member init =
        let regs = [W;X;Y;Z] |> List.map (fun r -> (r,CONST 0L)) |> Map
        ALU(regs,-1)
    

printProgram program

// trying to solve yet again

let rec solve (program:Program) =
    let rec eval (alu:ALU) (program:Program) =
        match program with
        | [] -> alu
        | inst::rest -> 
            printfn $"ALU {alu}"
            let alu = alu.exec inst
            printfn $"ALU {alu}"
            eval alu rest 
        
    let alu = ALU.init
    eval alu program
    
solve program 