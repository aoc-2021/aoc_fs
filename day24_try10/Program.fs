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
    | EMPTY 

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
    | EMPTY -> "🤬"

let toSequence (value:Value) =
    match value with
    | CONST c -> SEQUENCE [CONST c]
    | RANGE(a,b) -> [a..b] |> List.map CONST |> SEQUENCE 

let expand (value:Value) =
    match value with
    | _ -> value 
let consolidate (value:Value) =
    let rec consSeqList (values: list<Value>) =
        match values with
        | [] -> []
        | [_] -> values
        | CONST a::CONST b::rest when a + 1L = b -> consSeqList (RANGE (a,b)::rest)
        | CONST c::RANGE(a,b)::rest when c+1L = a -> consSeqList (RANGE (c,b)::rest)
        | RANGE (a,b)::CONST c::rest when b+1L = c -> consSeqList (RANGE (a,c)::rest)
        | RANGE (a,b)::RANGE(c,d)::rest when b + 1L = c -> consSeqList (RANGE (a,d)::rest)
        | a::rest -> a::(consSeqList rest)
    match value with
    | UNKNOWN -> UNKNOWN
    | CONST _ -> value
    | RANGE _ -> value
    | SEQUENCE [] -> EMPTY
    | SEQUENCE [v] -> v
    | SEQUENCE list -> consSeqList list |> SEQUENCE
    | FROM _ -> value 
    | TO _ -> value  
    | _ -> failwith $"consolidate: not implemented: {value}"

let rec canContain (value: Value) (i: int64) =
    match value with
    | UNKNOWN -> true
    | RANGE(a,b) -> a <= i && b >= i
    | FROM a -> a <= i
    | TO a -> a >= i
    | SEQUENCE l -> l |> List.exists (fun e -> canContain e i)
    | _ -> failwith $"canContain: Not implemented {value} {i}"

let isOneAndZero (value:Value) =
    canContain value 0L && canContain value 1L

let toBoolean (value:Value) : Value =
    match canContain value 0L,canContain value 1L with
    | true,true -> ONE_AND_ZERO
    | true,false -> CONST 0L
    | false,true -> CONST 1L
    | false,false -> EMPTY 

let rec intersection (value1: Value) (value2: Value) : Value =
    match value1, value2 with
    | UNKNOWN, _ -> value2
    | _, UNKNOWN -> value1
    | CONST a,CONST b when a = b -> CONST a
    | CONST a,CONST b when a <> b -> EMPTY
    | CONST c, RANGE (a,b) when c >= a && c <= b -> CONST c
    | CONST c, RANGE (a,b) when c < a || c > b  -> EMPTY
    | CONST c, FROM a when a <= c -> CONST c
    | CONST c, FROM a when a > c -> EMPTY 
    | RANGE (a,b),CONST c when c >= a && c <= b -> CONST c
    | RANGE (a,b),CONST c when c < a || c > b -> EMPTY 
    | RANGE (a,b),RANGE (c,d) ->
        let first = max a c
        let last = min b d
        if first > last then EMPTY 
        elif first = last then CONST first
        else RANGE(first,last)
    | SEQUENCE seq1,SEQUENCE seq2 ->
        let rec interSeq (seq1:list<Value>) (seq2:list<Value>) =
            match seq1,seq2 with
            | [],_ -> []
            | _,[] -> []
            | RANGE(a,b)::rest1,_ when a = b -> interSeq ((CONST a)::rest1) seq2
            | _,RANGE(a,b)::rest2 when a = b -> interSeq seq1 ((CONST a)::rest2)
            | CONST a::rest1,CONST b::_ when a < b -> interSeq rest1 (seq2)
            | CONST a::_,CONST b::rest2 when a > b -> interSeq seq1 rest2
            | CONST a::rest1,CONST b::rest2 when a = b -> CONST a :: (interSeq rest1 rest2)
            | CONST c::rest1,RANGE(a,b)::_ when c < a -> interSeq rest1 seq2
            | CONST c::rest1,RANGE(a,b)::rest2 when c >= a && c < b -> CONST c :: (interSeq rest1 (RANGE (c+1L,b)::rest2))
            | CONST c::rest1,RANGE(a,b)::rest2 when c = b -> CONST c :: (interSeq rest1 rest2)
            | CONST c::_,RANGE(a,b)::rest2 when b < c -> interSeq seq1 rest2
            | RANGE (a,b)::_,CONST c::rest2 when c < a -> interSeq seq1 rest2
            | RANGE (a,b)::rest1,CONST c::rest2 when c >= a && c < b -> CONST c :: (interSeq (RANGE ((c+1L),b)::rest1) rest2)
            | RANGE (a,b)::rest1,CONST c::rest2 when c = b -> CONST c::(interSeq rest1 rest2)
            | RANGE (a,b)::rest1,CONST c::_ when c > b -> interSeq rest1 seq2
            | RANGE (a,b)::rest1,RANGE (c,d)::rest2 when a = c && b = d -> RANGE(a,b)::(interSeq rest1 rest2)
        interSeq seq1 seq2 |> SEQUENCE
    | FROM a, FROM b -> FROM (max a b) 
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

let rec addValue (value: Value) (i: int64) =
    match value with
    | UNKNOWN -> UNKNOWN
    | CONST c -> CONST (c + i) 
    | SEQUENCE seq -> seq |> List.map (fun e -> addValue e i) |> SEQUENCE 
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

let rec minValue (value:Value) : int64 =
    match value with
    | CONST c -> c
    | RANGE (a,_) -> a
    | SEQUENCE (a::_) -> minValue a
    | FROM a -> a
    | _ -> failwith $"minValue: unsupported: {value}"

let rec narrowValues (op: Op) (param1: Value) (param2: Value) (result: Value) : Op * Value * Value * Value =
    // printfn $"narrow {op} {param1 |> valueToString} {param2 |> valueToString} {result |> valueToString}"
    printfn 
    let skipSilent() =
        op,param1,param2,result
    let param1 = consolidate param1
    let param2 = consolidate param2
    let result = consolidate result
    match op,param1,param2,result with
    | ADD,UNKNOWN,UNKNOWN,_ -> skipSilent ()
    | ADD,UNKNOWN,_,UNKNOWN -> skipSilent () 
    | ADD,_,UNKNOWN,UNKNOWN -> skipSilent ()
    | ADD,CONST 0L,_,_ -> 
        let value = intersection param2 result
        ADD,param1,value,value
    | ADD,RANGE (a,b),CONST c,_ ->
        let result = intersection (RANGE (a+c,b+c)) result
        let param1 =
            match result with
            | CONST r -> CONST (r-c)
            | RANGE (a,b) -> RANGE (a-c,b-c)
            | _ -> failwith "Not implemented Add: {param1} {param2}"
        // TODO: narrow param1
        ADD,param1,param2,result
    | ADD,CONST a,CONST b,_ -> ADD,param1,param2,intersection (CONST (a+b)) result
    | ADD,SEQUENCE _,CONST i,_ ->
        let result = intersection (addValue param1 i) result
        let param1 = addValue result (-i)
        ADD,param1,param2,result
    | DIV,UNKNOWN,_,UNKNOWN -> skipSilent () 
    | DIV,_,CONST 1L,_ ->
        let value = intersection param1 result
        DIV,value,CONST 1L,value
    | DIV,FROM a,CONST c,_ when a >= 0L && c > 0L ->
        let result = intersection (FROM (a/c)) result
        let param1 = FROM (minValue result * c)
        DIV,param1,param2,result
    | MUL,UNKNOWN,UNKNOWN,_ -> skipSilent ()
    | MUL,_,UNKNOWN,UNKNOWN -> skipSilent ()
    | MUL,UNKNOWN,CONST 0L,UNKNOWN -> MUL,UNKNOWN,CONST 0L,CONST 0L
    | MUL,UNKNOWN,_,UNKNOWN -> skipSilent ()
    | MUL,_,CONST 0L,_ -> MUL,param1,CONST 0L,CONST 0L
    | MUL,CONST 0L,_,_ ->
        let result = intersection (CONST 0L) result
        MUL,param1,param2,result
    | MUL,CONST c,RANGE(a,b),_ when c > 1L ->
        let result:Value = [a..b] |> List.map (((*) c) >> CONST) |> SEQUENCE |> intersection result
        // TODO; filter param2
        MUL,param1,param2,result
    | MUL,FROM 0L,_,_ when minValue param2 > 0L ->
        let result = intersection (FROM 0L) result
        let param1 = FROM (if canContain result 0L then 0L else 1L)
        MUL,param1,param2,result 
    | MOD,CONST 0L,CONST c,_ -> MOD,param1,param2,intersection result (CONST 0L)
    | MOD,UNKNOWN,_,_ -> MOD,FROM 0L,param2,result 
    | MOD,UNKNOWN,CONST c,_ -> MOD,param1,param2,intersection (RANGE (0L,c-1L)) result
    | MOD,FROM _,_,_ -> MOD,param1,param2,result 
    | EQL,UNKNOWN,UNKNOWN,_ -> EQL,UNKNOWN,UNKNOWN,toBoolean result 
    | EQL,UNKNOWN,_,UNKNOWN -> EQL,param1,param2,ONE_AND_ZERO
    | EQL,_,_,_ when isOneAndZero result -> EQL,param1,param2,toBoolean result 
     
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
    { 1 .. 17 }
    |> Seq.fold (fun program i ->
                                 printfn $"### ITER {i} ###"
                                 task1iter program) program
let program1 = task1 program

printProgram program1

