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

type Op =
    | I_INP
    | I_ADD
    | I_MUL
    | I_DIV
    | I_MOD
    | I_SET
    | I_NOP
    | I_EQL

type Param =
    | R of Reg
    | I of int64
    | NA

let paramToString (param:Param) =
    match param with
    | R reg -> $"{reg} "
    | I i -> $"{i}"
    | NA -> ""

type Instruction(op:Op,reg:Reg,param:Param) =
    member this.Op = op
    member this.Reg = reg
    member this.Param = param
    override this.ToString () =
        $"{op} {reg} {param |> paramToString}"

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
    | [| "add"; r; p |] -> Instruction(I_ADD,(r |> toReg),(p |> toParam))
    | [| "mul"; r; p |] -> Instruction(I_MUL,(r |> toReg),(p |> toParam))
    | [| "div"; r; p |] -> Instruction(I_DIV,(r |> toReg),(p |> toParam))
    | [| "mod"; r; p |] -> Instruction(I_MOD,(r |> toReg),(p |> toParam))
    | [| "eql"; r; p |] -> Instruction(I_EQL,(r |> toReg),(p |> toParam))
    | [| "inp"; r |] -> Instruction(I_INP,(r |> toReg),NA)

let program = file |> List.map parseLine


type Value =
    | INPUT of int
    | SUM of Value*Value
    | MUL of Value
    | MOD of Value
    | DIV of Value
    | CONST of int64 
    
type ALU(regs:Map<Reg,Value>) =
    member this.Regs = regs
    member this.get (reg:Reg) = regs.TryFind reg |>Option.get
    member this.set (reg:Reg) (value:Value) =
        regs.Add(reg,value) |> ALU

    static member init = [W;X;Y;Z] |> List.map (fun r -> (r,CONST 0L)) |> Map |> ALU 

printProgram program 