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
    | NATURAL
    | POSITIVE
    | NEGATIVE
    | NOT_ZERO
    | RANGE of int64 * int64
    | FROM of int64
    | TO of int64
    | VALUES of Set<int64>
    | CONST of int64

let ALL_INPUTS = [ 1L .. 9L ] |> Set |> VALUES
let ONE_AND_ZERO = [ 0L; 1L ] |> Set |> VALUES

let valueToString (value: Value) =
    match value with
    | UNKNOWN -> "?"
    | NATURAL -> "ℕ"
    | POSITIVE -> ">0"
    | NEGATIVE -> "<0"
    | NOT_ZERO -> "!0"
    | RANGE (a, b) -> $"[{a}..{b}]"
    | VALUES s ->
        s
        |> Set.toList
        |> List.map string
        |> String.concat " "
        |> sprintf "{%s}"
    | CONST c -> c |> string
    | FROM c -> $"[{c},∞⟩"
    | TO c -> $"⟨-∞,{c}]"

let expand (value:Value) =
    match value with
    | RANGE (a,b) -> [a..b] |> Set |> VALUES
    | _ -> value 

let consolidate (value:Value) =
    match value with
    | UNKNOWN -> UNKNOWN
    | CONST c -> CONST c 
    | NATURAL -> NATURAL
    | POSITIVE -> POSITIVE
    | NOT_ZERO -> NOT_ZERO
    | RANGE (a,b) when a=b -> CONST a
    | RANGE (a,b) when b-a < 10 -> [a..b] |> Set |> VALUES
    | RANGE _ -> value 
    | VALUES s when s.Count > 40 -> RANGE (s |> Set.toList |> List.min, s |> Set.toList |> List.max )
    | VALUES _ -> value
    | FROM 0L -> NATURAL
    | FROM 1L -> POSITIVE
    | FROM _ -> value 
    | _ -> failwith $"consolidate: not implemented: {value}"

let canContain (value: Value) (i: int64) =
    match value with
    | UNKNOWN -> true
    | CONST c -> c = i
    | NATURAL -> i >= 0L
    | POSITIVE -> i > 0L
    | NOT_ZERO -> i <> 0L
    | RANGE (a, b) -> i >= a && i <= b
    | VALUES s -> s.Contains i
    | TO upper -> i <= upper
    | FROM c -> i >= c
    | _ -> failwith $"canContain: Not implemented {value} {i}"

let rec intersection (value1: Value) (value2: Value) =
    let isPositive (i: int64) = i > 0L

    match value1, value2 with
    | UNKNOWN, _ -> value2
    | _, UNKNOWN -> value1
    | CONST c, _ when canContain value2 c -> CONST c
    | CONST c, _ -> failwith $"Invalid intersection {value1} {value2}"
    | _, CONST _ -> intersection value2 value1
    | NATURAL, NATURAL -> NATURAL
    | _, NATURAL -> intersection value2 value1
    | NATURAL, VALUES vs -> vs |> Set.filter ((<) -1L) |> VALUES
    | NATURAL, RANGE (a,b) when a >= 0L -> value2
    | NATURAL, RANGE (a,b) when b < 0L -> failwith "$intersect NATURAL {value2} = Ø"
    | NATURAL, RANGE (a,b) when a < 0L && b >= 0L -> RANGE (0L,b)
    | POSITIVE, POSITIVE -> POSITIVE
    | POSITIVE, NOT_ZERO -> POSITIVE
    | POSITIVE, RANGE (a, b) when a > 0 -> value2
    | POSITIVE, RANGE (a, b) when b > 0 -> RANGE(1L, b)
    | POSITIVE, VALUES s -> s |> Set.filter isPositive |> VALUES
    | NOT_ZERO, NOT_ZERO -> NOT_ZERO
    | NOT_ZERO, _ -> intersection value2 value1
    | RANGE (a, b), RANGE (c, d) when a > d || b < c -> failwith $"No intersection: {value1} {value2}"
    | RANGE (a, b), RANGE (c, d) -> RANGE(max a c, min b d)
    | RANGE _, _ -> intersection value2 value1
    | VALUES s1, VALUES s2 -> Set.intersect s1 s2 |> VALUES
    | VALUES s1, _ ->
        s1
        |> Set.filter (fun i -> canContain value2 i)
        |> VALUES
    | TO a, TO b -> TO(min a b)
    | NATURAL,FROM i when i > 0L -> value2
    | NATURAL,FROM i when i <= 0L -> NATURAL
    | FROM a,FROM b -> FROM (max a b)
    | FROM from,RANGE (a,_) when a > from -> value2
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
        | I i -> CONST i
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
    | CONST c -> CONST (c+i)
    | RANGE(a,b) -> RANGE (a+i,b+i)
    | VALUES s -> s |> Set.map ((+) i) |> VALUES 
    | _ -> failwith $"Not implemented: + {value} {i}"

let mulValue (value: Value) (i: int64) : Value =
    match value with 
    _ -> failwith $"Not implemented * {value} {i}"
let divValue (value: Value) (i: int64) : Value =
    match value with 
    _ -> failwith $"Not implemented / {value} {i}"

let rec xorValues (value1: Value) (value2: Value) : Value =
    match value1,value2 with
    | CONST a, CONST b when a = b -> value1
    | CONST a, CONST b when a <> b -> failwith "⊻ {value1} {value2} -> ∅" 
    | _,_ -> failwith $"Not implemented ⊻ {value1} {value2}"

let rec eqValue (value1: Value) (value2: Value) : Value =
    let res = match value1,value2 with
              | UNKNOWN,_ -> ONE_AND_ZERO
              | _,UNKNOWN -> ONE_AND_ZERO
              | CONST a,CONST b when a = b -> CONST 1L
              | CONST a,CONST b when a <> b -> CONST 0L 
              | VALUES s, CONST c when not (s.Contains c) -> CONST 0L
              | VALUES s, CONST c when s.Contains c -> ONE_AND_ZERO
              | VALUES s1, VALUES s2 when Set.intersect s1 s2 |> Set.isEmpty |> not -> ONE_AND_ZERO
              | VALUES s1, VALUES s2 when Set.intersect s1 s2 |> Set.isEmpty -> CONST 0L
              | _,VALUES s1 -> eqValue value2 value1
              | VALUES s1,_ when s1 |> Set.exists (fun i -> canContain value2 i) -> ONE_AND_ZERO
              | VALUES s1,_ when s1 |> Set.filter(fun i -> canContain value2 i) |> Set.isEmpty -> CONST 0L 
              | _,_ -> failwith $"Not implemented EQ {value1} {value2}"
    // printfn $"eqValue {value1} {value2} -> {res}"
    res 

let rec removeFromValue (value1:Value) (value2:Value) =
    match value1,value2 with
    | _,_ when eqValue value1 value2 = CONST 0L -> value1 
    | UNKNOWN,CONST 0L -> NOT_ZERO
    | UNKNOWN,_ -> UNKNOWN
    | _,UNKNOWN -> value1
    | VALUES s, _ -> s |> Set.filter (fun i -> canContain value2 i |> not) |> VALUES
    | RANGE (a,b),_ when b-a >= 30 -> value1
    | RANGE (a,b),_ when b-a < 30 -> removeFromValue (expand value1) value2 
    | _ -> failwith $"Not implemented {value1} - {value2}"

let rec narrowValues (op: Op) (param1: Value) (param2: Value) (result: Value) : Op * Value * Value * Value =
    let param1 = consolidate param1
    let param2 = consolidate param2
    let result = consolidate result
    
    if param1 = VALUES Set.empty || param2 = VALUES Set.empty || result = VALUES Set.empty then
        // failwith $"narrowValues EMPTY {op} {param1} {param2} {result}"
        printfn $"narrowValues EMPTY {op} {param1} {param2} {result}"
    
    let skip () =
        printfn $"Skipping: {op} {param1} {param2} {result}"
        op,param1,param2,result 
    match op, param1, param2, result with
    | ADD,_,CONST 0L,_ ->
        let value = intersection param1 result
        ADD,value,param2,value
    | ADD,CONST a,CONST b,_ ->
        let result = intersection (CONST(a+b)) result
        ADD,param1,param2,result 
    | ADD,CONST 0L,_,_ ->
        let value = intersection param2 result
        ADD,param1,value,value
    | ADD,_,CONST c,_ ->
        let result = intersection (addValue param1 c) result
        let param1 = intersection (addValue result -c) param1
        ADD,param1,param2,result
    | ADD,_,VALUES v,CONST 0L ->
        let v = v |> Set.filter (fun i -> canContain param1 (-i))
        let param2 = VALUES v
        let param1 = intersection (v |> Set.map ((*) -1L) |> VALUES) param1
        ADD,param1,param2,result
    | ADD,VALUES s1,VALUES s2,_ ->
        let pairs = List.allPairs (s1 |>Set.toList) (s2 |> Set.toList)
        let pairs = pairs |> List.filter (fun (a,b) -> a+b |> (canContain result))
        let param1 = pairs |> List.map fst |> Set |> VALUES
        let param2 = pairs |> List.map snd |> Set |> VALUES
        let result = pairs |> List.map (fun (a,b) -> a+b) |> Set |> VALUES // |> consolidate
        ADD,param1,param2,result
    | ADD,NATURAL,VALUES s1,NATURAL ->
        let smallest = s1 |> Set.toList |> List.min
        let result = if smallest = 0L then NATURAL else FROM smallest
        ADD,param1,param2,result
    | MUL,_,CONST 0L,_ ->
        MUL,param1,param2,(intersection (CONST 0L) result)
    | MUL,CONST 0L,_,_ ->
        MUL,param1,param2,(intersection (CONST 0L) result)
    | MUL,_,CONST 1L,_ ->
        let value = intersection param1 result
        MUL,value,param2,value
    | MUL,CONST c,VALUES v,_ ->
        let result = intersection (v |> Set.map ((*) c) |> VALUES) result
        let param2 = v |> Set.filter (fun i -> i * c |> (canContain result)) |> VALUES
        MUL,param1,param2,result
    | MUL,VALUES v, CONST c,_ ->
        let result = intersection (v |> Set.map ((*) c) |> VALUES) result
        let param1 = v |> Set.filter (fun i -> i * c |> (canContain result)) |> VALUES
        MUL,param1,param2,result
    | MUL,VALUES s1,VALUES s2,_ ->
        let pairs = List.allPairs (s1 |>Set.toList) (s2 |> Set.toList)
        let pairs = pairs |> List.filter (fun (a,b) -> a*b |> (canContain result))
        let param1 = pairs |> List.map fst |> Set |> VALUES
        let param2 = pairs |> List.map snd |> Set |> VALUES
        let result = pairs |> List.map (fun (a,b) -> a*b) |> Set |> VALUES // |> consolidate
        MUL,param1,param2,result
    | MUL,NATURAL,RANGE(a,b),_ when a >= 0 ->
        MUL,param1,param2,intersection result NATURAL
    | MUL,NATURAL,VALUES s1,_ ->
        let isNatural = s1 |> Set.exists ((fun i -> i < 0L)) |> not
        let result = if isNatural then NATURAL else UNKNOWN
        MUL,param1,param2,result 
    | DIV,_,CONST 1L,_ ->
        let value = intersection param1 result
        DIV,value,param2,value
    | DIV,NATURAL,CONST c,_ when c > 0L ->
        DIV,param1,param2,intersection result NATURAL 
    | MOD,CONST 0L,_,_ ->
        MOD,param1,param2,intersection (CONST 0L) result
    | MOD,VALUES v,CONST c,_ ->
        let result = intersection (v |> Set.map (fun i -> i % c) |> VALUES) result
        let param1 = v |> Set.filter (fun i -> i % c |> (canContain result)) |> VALUES 
        MOD,param1,param2,result
    | MOD,UNKNOWN,CONST c,_ ->
        let result = intersection (RANGE(0L,c-1L)) result
        MOD,NATURAL,param2,result  // TODO: narrow param1
    | EQL,_,_,CONST 1L ->
        let value = intersection param1 param2
        EQL,value,value,result 
    | EQL,_,_,_ ->
        let result = intersection (eqValue param1 param2) result
        EQL,param1,param2,result  
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
        | NOP -> $"STEP: {op}        in={before} out={after}"
        | _ -> $"STEP: {op} {reg} {param |> paramToString, 3}  in={before} out={after}"

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

let readProgram (input: list<string>) : Program =
    let first::rest = file |> List.map parseLine
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
    { 1 .. 100 }
    |> Seq.fold (fun program i ->
                                 printfn $"### ITER {i} ###"
                                 task1iter program) program
let program1 = task1 program

printProgram program1


let _123 = [1L;2L;3L] |> Set |> VALUES
let _789 = [7L;8L;9L] |> Set |> VALUES

let test1= narrowValues ADD _123 _789 NATURAL 
printfn $"test1: {test1}"

printfn $"eq {eqValue (CONST 0L) ONE_AND_ZERO} "
