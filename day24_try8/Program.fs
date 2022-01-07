open System.IO

let file = File.ReadAllLines "input.txt" |> Array.toList 

file |> List.map (printfn "%A")

type Reg = W | X | Y | Z

let ALL_REGS = [W;X;Y;Z]
let otherRegs (reg:Reg) = ALL_REGS |> List.filter ((<>) reg)

type Value =
    | UNKNOWN
    | NATURAL
    | POSITIVE
    | NEGATIVE
    | NOT_ZERO
    | RANGE of int64*int64
    | FROM of int64
    | TO of int64 
    | VALUES of Set<int64>
    | CONST of int64
    | UNUSED 

let ALL_INPUTS = [1L..9L] |> Set |> VALUES
let ONE_AND_ZERO = [0L;1L] |> Set |> VALUES
    
let valueToString (value:Value) =
    match value with
    | UNKNOWN -> "?"
    | NATURAL -> "ℕ"
    | POSITIVE -> ">0"
    | NEGATIVE -> "<0"
    | NOT_ZERO -> "!0"
    | RANGE (a,b) -> $"[{a}..{b}]"
    | VALUES s -> s |> Set.toList |> List.map string |> String.concat " " |> sprintf "{%s}"
    | CONST c -> c |> string
    | FROM c -> $"[{c},∞⟩"
    | TO c -> $"⟨-∞,{c}]"
    | UNUSED -> "⟤"

let canContain (value:Value) (i:int64) =
    match value with
    | UNKNOWN -> true
    | CONST c -> c = i 
    | NATURAL -> i >= 0L
    | POSITIVE -> i > 0L
    | NOT_ZERO -> i <> 0L
    | RANGE (a,b) -> i >= a && i <= b
    | VALUES s -> s.Contains i
    | UNUSED -> true 

let rec intersection (value1:Value) (value2:Value) =
    // printfn $"intersection {value1} {value2}"
    let isPositive (i:int64) = i > 0L
    match value1,value2 with
    | UNKNOWN,_ -> value2
    | _,UNKNOWN -> value1 
    | CONST c, _ when canContain value2 c -> CONST c 
    | CONST c, _ -> failwith "$Invalid intersection {value1} {value2}"
    | _, CONST _ -> intersection value2 value1
    | NATURAL,NATURAL -> NATURAL
    | _, NATURAL -> intersection value2 value1
    | POSITIVE,POSITIVE -> POSITIVE
    | POSITIVE,NOT_ZERO -> POSITIVE
    | POSITIVE,RANGE (a,b) when a > 0 -> value2
    | POSITIVE,RANGE (a,b) when b > 0 -> RANGE(1L,b)
    | POSITIVE,VALUES s -> s |> Set.filter isPositive |> VALUES
    | NOT_ZERO,NOT_ZERO -> NOT_ZERO
    | NOT_ZERO,_ -> intersection value2 value1
    | RANGE (a,b),RANGE(c,d) when a > d || b < c -> failwith $"No intersection: {value1} {value2}"
    | RANGE (a,b),RANGE(c,d) -> RANGE (max a c,min b d)
    | RANGE _,_ -> intersection value2 value1
    | VALUES s1,VALUES s2 -> Set.intersect s1 s2 |> VALUES 
    | VALUES s1,_ -> s1 |> Set.filter (fun i -> canContain value2 i) |> VALUES
    | _,UNUSED -> UNUSED 


type Op =
    | INP
    | ADD
    | MUL
    | DIV
    | MOD
    | SET
    | NOP
    | EQL 

type Param =
    | R of Reg
    | I of int64
    | NA         
type ALU (regs:Map<Reg,Value>) =
    member this.Regs = regs
    member this.get (reg:Reg) = regs.TryFind reg |> Option.get
    member this.getValue (param:Param) =
        match param with
        | I i -> CONST i
        | R reg -> this.get reg
        
    member this.set (reg:Reg) (value:Value) = ALU(regs.Add(reg,value))
    
    member this.SyncValues (regs:list<Reg>) (other:ALU) : ALU*ALU =
        let syncReg (reg:Reg) (alu1:ALU) (alu2:ALU) =
            let value = intersection (alu1.get reg) (alu2.get reg)
            let alu1 = alu1.set reg value
            let alu2 = alu2.set reg value
            alu1,alu2
        regs |> List.fold (fun (alu1,alu2) (reg:Reg) -> syncReg reg alu1 alu2) (this,other)
             
            
    static member unknown = [W;X;Y;Z] |> List.map (fun r -> r,UNKNOWN) |> Map |> ALU 
    static member initial = [W;X;Y;Z] |> List.map (fun r -> r,CONST 0L) |> Map |> ALU
    
    override this.ToString () =
        [W;X;Y;Z] 
        |> List.map (fun r -> r,regs.TryFind r |> Option.get |> valueToString)
        |> List.map (fun (r,v) -> $"{r}={v}") |> String.concat " "
        |> sprintf "ALU[{%s}]"
    
let paramToString (param:Param) =
    match param with
    | R reg -> reg |> string
    | I i -> i |> string
    | NA -> " "

let addToValue (value:Value) (i:int64) =
    match value with
    | UNKNOWN -> UNKNOWN
    | CONST c -> CONST (c + i)
    | NATURAL -> FROM i
    | POSITIVE -> FROM (i+1L)
    | NOT_ZERO -> UNKNOWN
    | RANGE (a,b) -> RANGE (a+i,b+i)
    | VALUES s -> s |> Set.map ((+) i) |> VALUES 
    | _ -> failwith $"Not implemented: {value} {i}"
    
    
let rec narrowValues (op:Op) (param1:Value) (param2:Value) (result:Value) : Op*Value*Value*Value =
    printfn $"narrowValues {op} {param1} {param2} {result}"
    match op,param1,param2,result with
    | NOP,_,_,_ ->
        let inter = intersection param1 result
        op,inter, param2, inter
    | ADD,_,_,UNUSED -> narrowValues NOP param1 param2 result 
    | ADD,_,CONST 0L,_ -> narrowValues NOP param1 param2 result
    | ADD,CONST 0L,_,_ -> narrowValues SET param1 param2 result 
    | ADD,_,CONST c,_ ->
        let result = intersection (addToValue param1 c) result
        let param1 = intersection (addToValue result (-c)) param1
        op,param1,param2,result
    | MUL,_,_,UNUSED -> narrowValues NOP param1 param2 result 
    | MUL,_,CONST 0L,_ -> narrowValues SET param1 (CONST 0L) result
    | DIV,_,CONST 1L,_ -> narrowValues NOP param1 param2 result 
    | MOD,UNKNOWN,_,_ -> narrowValues MOD NATURAL param2 result
    | MOD,_,CONST c,UNKNOWN -> narrowValues MOD param1 param2 (RANGE (0,c-1L))
    | EQL,_,_,UNUSED -> narrowValues NOP param1 param2 result 
    | EQL,UNKNOWN,UNKNOWN,UNKNOWN -> EQL,UNKNOWN,UNKNOWN,ONE_AND_ZERO
    | EQL,_,_,UNKNOWN -> narrowValues EQL param1 param2 ONE_AND_ZERO
    | SET,CONST a,CONST b,_ when a = b -> narrowValues NOP UNUSED param2 result
    | SET,_,_,UNUSED -> narrowValues NOP param1 param2 result 
    | SET,_,_,_ ->
        let value = intersection result param2
        op,UNUSED,value,value 
    | _,UNKNOWN,UNKNOWN,UNKNOWN -> op,UNKNOWN,UNKNOWN,UNKNOWN
    | _ ->
        printfn $"Not handled: {op}"
        op,param1,param2,result 
type Step (op:Op,reg:Reg,param:Param,before:ALU,after:ALU) =
    member this.Op = op
    member this.Reg = reg
    member this.Param = param
    member this.Before = before
    member this.After = after
    
    member this.setBefore (before:ALU) = Step (op,reg,param,before,after)
    member this.setAfter (after:ALU) = Step (op,reg,param,before,after)
    
    member this.narrow () =
        match param with
        | R r2 -> 
            let v1 = before.get reg
            let v2 = before.get r2
            let res = after.get reg
            let op,v1,v2,res = narrowValues op v1 v2 res
            let before = before.set reg v1
            let before = before.set r2 v2
            let after = after.set reg res
            let before,after = before.SyncValues (otherRegs reg) after 
            Step(op,reg,R r2,before,after)
        | I v2 -> 
            let v1 = before.get reg
            let res = after.get reg
            let op,v1,_,res = narrowValues op v1 (CONST v2) res
            let before = before.set reg v1
            let after = after.set reg res
            let before,after = before.SyncValues (otherRegs reg) after 
            Step(op,reg,I v2,before,after)
        | NA -> // input
            assert (op = INP)
            let value = after.get reg
            let value = intersection ALL_INPUTS value
            let after = after.set reg value
            let before = before.set reg UNUSED
            let before,after = before.SyncValues (otherRegs reg) after 
            Step(op,reg,NA,before,after)
        
    
    static member init (op:Op) (reg:Reg) (param:Param) =
        Step (op,reg,param,ALU.unknown,ALU.unknown)
        
    override this.ToString () =
        $"STEP: {op} {reg} {param |> paramToString}  in={before} out={after}"
        
type Program = list<Step> 

let parseLine (line:string) : Step =
    let line = line.Split ' '
    let toReg (s:string) : Reg =
        match s with
        | "w" -> W
        | "x" -> X
        | "y" -> Y
        | "z" -> Z
        | _ -> failwith $"not a register: {s}"
    let toParam (s:string) =
        match s with
        | "w" -> R W
        | "x" -> R X
        | "y" -> R Y
        | "z" -> R Z
        | _ -> s |> int64 |> I 
    match line with
    | [|"add";r;p|] -> Step.init ADD (r|>toReg) (p|>toParam)
    | [|"mul";r;p|] -> Step.init MUL (r|>toReg) (p|>toParam)
    | [|"div";r;p|] -> Step.init DIV (r|>toReg) (p|>toParam)
    | [|"mod";r;p|] -> Step.init MOD (r|>toReg) (p|>toParam)
    | [|"eql";r;p|] -> Step.init EQL (r|>toReg) (p|>toParam)
    | [|"inp";r|]   -> Step.init INP (r|>toReg) NA 

let readProgram (input:list<string>) : Program = 
    let first::rest = file |> List.map parseLine
    (first.setBefore ALU.initial) :: rest

let printProgram (program:Program) =
    program |> List.map (printfn "%A") |> ignore     

let program = readProgram file 

printProgram program

printfn "Solving:"

let setLastZToOne (program:Program) =
    let last::rest = program |> List.rev
    let after = last.After.set Z (CONST 0L)
    let last = last.setAfter after
    let program = last :: rest
    program |> List.rev 

let narrowEachStep (program:Program) =
    program |> List.map (fun step -> step.narrow ())

let rec syncBetweenSteps (program:Program) =
    match program with
    | [] -> []
    | [_] -> program
    | step1::step2::rest ->
        let alu,_ = step1.After.SyncValues ALL_REGS step2.Before
        let step1 = step1.setAfter alu
        let step2 = step2.setBefore alu
        step1 :: (syncBetweenSteps (step2::rest))
       
let task1iter (program:Program) =
    let program = setLastZToOne program
    let program = narrowEachStep program
    let program = syncBetweenSteps program 
    program 

let task1 (program:Program) =
    program 
    |> task1iter
    |> task1iter
    |> task1iter

let program1 = task1 program 

printProgram program1
