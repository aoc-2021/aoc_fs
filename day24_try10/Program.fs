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

type Value =
    | UNKNOWN
    | FROM of int64
    | TO of int64
    | CONST of int64 
    | RANGE of int64*int64
    | SEQUENCE of list<Value> 

let ALL_INPUTS = RANGE (1,9)
let ONE_AND_ZERO = RANGE (0,1)

let rec valueToString (value: Value) =
    match value with
    | UNKNOWN -> "?"
    | CONST c -> c |> string
    | RANGE (a,b) -> $"[{a}…{b}]"
    | TO c -> $"⟨∞,{c}]"
    | FROM c -> $"[{c},∞⟩"
    | SEQUENCE l -> l |> List.map valueToString |> String.concat "·" |> sprintf "【%s】"

let expand (value:Value) =
    match value with
    | _ -> value 

let consolidate (value:Value) =
    match value with
    | UNKNOWN -> UNKNOWN
    | CONST _ -> value
    | RANGE _ -> value 
    | _ -> failwith $"consolidate: not implemented: {value}"

let canContain (value: Value) (i: int64) =
    match value with
    | UNKNOWN -> true
    | _ -> failwith $"canContain: Not implemented {value} {i}"

let rec intersection (value1: Value) (value2: Value) =
    let isPositive (i: int64) = i > 0L

    match value1, value2 with
    | UNKNOWN, _ -> value2
    | _, UNKNOWN -> value1
    | _ -> failwith $"intersection: Not implemented: {value1} {value2}"


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

type ALU(regs: Map<Reg, Value>) =
    member this.Regs = regs
    member this.get(reg: Reg) = regs.TryFind reg |> Option.get

    member this.getValue(param: Param) =
        match param with
        | R reg -> this.get reg

    member this.set (reg: Reg) (value: Value) = ALU(regs.Add(reg, value))

    member this.SyncValues (regs: list<Reg>) (other: ALU) : ALU * ALU =
        let syncReg (reg: Reg) (alu1: ALU) (alu2: ALU) =
            let value =
                intersection (alu1.get reg) (alu2.get reg)

            let alu1 = alu1.set reg value
            let alu2 = alu2.set reg value
            alu1, alu2

        regs
        |> List.fold (fun (alu1, alu2) (reg: Reg) -> syncReg reg alu1 alu2) (this, other)


    static member unknown =
        [ W; X; Y; Z ]
        |> List.map (fun r -> r, UNKNOWN)
        |> Map
        |> ALU

    static member initial =
        [ W; X; Y; Z ]
        |> List.map (fun r -> r, CONST 0L)
        |> Map
        |> ALU

    override this.ToString() =
        [ W; X; Y; Z ]
        |> List.map (fun r -> r, regs.TryFind r |> Option.get |> valueToString)
        |> List.map (fun (r, v) -> $"{r}={v}")
        |> String.concat " "
        |> sprintf "ALU[{%s}]"

let paramToString (param: Param) =
    match param with
    | R reg -> reg |> string
    | I i -> i |> string
    | NA -> " "

let addValue (value: Value) (i: int64) =
    match value with
    | UNKNOWN -> UNKNOWN
    | _ -> failwith $"Not implemented: + {value} {i}"

let mulValue (value: Value) (i: int64) : Value =
    match value with 
    _ -> failwith $"Not implemented * {value} {i}"
let divValue (value: Value) (i: int64) : Value =
    match value with 
    _ -> failwith $"Not implemented / {value} {i}"

let rec xorValues (value1: Value) (value2: Value) : Value =
    match value1,value2 with
    | _,_ -> failwith $"Not implemented ⊻ {value1} {value2}"

let rec eqValue (value1: Value) (value2: Value) : Value =
    let res = match value1,value2 with
              | UNKNOWN,_ -> ONE_AND_ZERO
              | _,UNKNOWN -> ONE_AND_ZERO
              | _,_ -> failwith $"Not implemented EQ {value1} {value2}"
    // printfn $"eqValue {value1} {value2} -> {res}"
    res 

let rec narrowValues (op: Op) (param1: Value) (param2: Value) (result: Value) : Op * Value * Value * Value =
    let param1 = consolidate param1
    let param2 = consolidate param2
    let result = consolidate result
    match op,param1,param2,result with 
    | _ ->
        printfn $"Not handled: {op} {param1 |> valueToString} {param2 |> valueToString} {result |> valueToString}"
        op, param1, param2, result

type Step(op: Op, reg: Reg, param: Param, before: ALU, after: ALU) =
    member this.Op = op
    member this.Reg = reg
    member this.Param = param
    member this.Before = before
    member this.After = after

    member this.setBefore(before: ALU) = Step(op, reg, param, before, after)
    member this.setAfter(after: ALU) = Step(op, reg, param, before, after)

    member this.narrow() =
        match param with
        | R r2 ->
            let v1 = before.get reg
            let v2 = before.get r2
            let res = after.get reg
            let op, v1, v2, res = narrowValues op v1 v2 res
            let before = before.set reg v1
            let before = before.set r2 v2
            let after = after.set reg res
            let before, after = before.SyncValues(otherRegs reg) after
            Step(op, reg, R r2, before, after)
        | I v2 ->
            let v1 = before.get reg
            let res = after.get reg
            let op, v1, _, res = narrowValues op v1 (CONST v2) res
            let before = before.set reg v1
            let after = after.set reg res
            let before, after = before.SyncValues(otherRegs reg) after
            Step(op, reg, I v2, before, after)
        | NA -> // input
            assert (op = INP)
            let value = after.get reg
            let value = intersection ALL_INPUTS value
            let after = after.set reg value
            let before, after = before.SyncValues(otherRegs reg) after
            Step(op, reg, NA, before, after)

    static member init (op: Op) (reg: Reg) (param: Param) =
        Step(op, reg, param, ALU.unknown, ALU.unknown)

    override this.ToString() =
        match op with
        | NOP -> $"STEP: {op}" //       ⟶ {after}" //       in={before} out={after}"
        | _ -> $"STEP: {op} {reg} {param |> paramToString, 3} ⟶ out={after}"

type Program = list<Step>

let parseLine (line: string) : Step =
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
    | [| "add"; r; p |] -> Step.init ADD (r |> toReg) (p |> toParam)
    | [| "mul"; r; p |] -> Step.init MUL (r |> toReg) (p |> toParam)
    | [| "div"; r; p |] -> Step.init DIV (r |> toReg) (p |> toParam)
    | [| "mod"; r; p |] -> Step.init MOD (r |> toReg) (p |> toParam)
    | [| "eql"; r; p |] -> Step.init EQL (r |> toReg) (p |> toParam)
    | [| "inp"; r |] -> Step.init INP (r |> toReg) NA
    | _ -> failwith $"Unrecognized input: {line}"

let readProgram (input: list<string>) : Program =
    let first::rest = input |> List.map parseLine
    (first.setBefore ALU.initial) :: rest

let printProgram (program: Program) =
    program |> List.map (printfn "%A") |> ignore

let program = readProgram file

// printProgram program

printfn "Solving:"

let setLastZToOne (program: Program) =
    let last::rest = program |> List.rev
    let after = last.After.set Z (CONST 0L)
    let last = last.setAfter after
    let program = last :: rest
    program |> List.rev

let narrowEachStep (program: Program) =
    program |> List.map (fun step -> step.narrow ())

let rec syncBetweenSteps (program: Program) =
    match program with
    | [] -> []
    | [ _ ] -> program
    | step1 :: step2 :: rest ->
        // printfn $"SYNC: {step1} {step2}"
        let alu, _ =
            step1.After.SyncValues ALL_REGS step2.Before

        let step1 = step1.setAfter alu
        let step2 = step2.setBefore alu
        step1 :: (syncBetweenSteps (step2 :: rest))

let task1iter (program: Program) =
    let program = setLastZToOne program
    let program = narrowEachStep program
    let program = syncBetweenSteps program
    program

let task1 (program: Program) =
    { 1 .. 1 }
    |> Seq.fold (fun program i ->
                                 printfn $"### ITER {i} ###"
                                 task1iter program) program
let program1 = task1 program

printProgram program1

