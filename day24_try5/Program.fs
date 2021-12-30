open System.IO

let file =
    File.ReadAllLines "input.txt" |> Array.toList

type Reg =
    | W
    | X
    | Y
    | Z

type Value =
    | CONST of int64
    | INPUT of int * Map<int, int64>
    | MULTIPLE of Set<int64>
    | RANGE of int64 * int64
    | INVALID

type Inst =
    | INP of Reg
    | ADDI of Reg * Value
    | ADDR of Reg * Reg
    | MULI of Reg * Value
    | MULR of Reg * Reg
    | DIVI of Reg * int64
    | MODI of Reg * int64
    | EQLI of Reg * Value
    | EQLR of Reg * Reg
    | SETI of Reg * Value
    | SETR of Reg * Reg

type ALU = Map<Reg,Value>

let ANY_INPUT (i: int) =
    let input =
        [ 1 .. 9 ]
        |> List.map (fun i -> (i, i |> int64))
        |> Map

    INPUT(i, input)

let fillALU a =
    [ (W, a); (X, a); (Y, a); (Z, a) ] |> Map

type Program = list<Inst>

let isReg (s: string) =
    [ "w"; "x"; "y"; "z" ] |> List.contains s

let toReg (s: string) =
    match s with
    | "w" -> W
    | "x" -> X
    | "y" -> Y
    | "z" -> Z

let parseLine (line: string) =
    match line.Split ' ' with
    | [| "inp"; reg |] -> INP(toReg reg)
    | [| "add"; reg; reg2 |] when isReg reg2 -> ADDR(toReg reg, toReg reg2)
    | [| "add"; reg; i |] -> ADDI(toReg reg, i |> int64 |> CONST)
    | [| "mul"; reg; reg2 |] when isReg reg2 -> MULR(toReg reg, toReg reg2)
    | [| "mul"; reg; i |] -> MULI(toReg reg, i |> int64 |> CONST)
    | [| "div"; reg; i |] -> DIVI(toReg reg, i |> int64)
    | [| "mod"; reg; i |] -> MODI(toReg reg, i |> int64)
    | [| "eql"; reg; reg2 |] when isReg reg2 -> EQLR(toReg reg, toReg reg2)
    | [| "eql"; reg; i |] -> EQLI(toReg reg, i |> int64 |> CONST)
    | _ -> failwith $"Unknown input: {line}"

let v2s (value: Value) =
    match value with
    | CONST i -> i |> string
    | INPUT (i, vals) ->
        let vals =
            [ 1 .. 9 ]
            |> List.map (vals.TryFind)
            |> List.map (Option.map string)
            |> List.map (Option.defaultValue "_")
            |> String.concat " "
        $"{i}#{vals}"
    | MULTIPLE s ->
        s
        |> Set.toList
        |> List.map string
        |> String.concat " "
        |> sprintf "{ %A }"
    | RANGE (a, b) -> $"[{a} .. {b} ]"

let printInstruction (inst: Inst) =
    match inst with
    | INP reg -> $"reg ⬅"
    | ADDI (reg, value) -> $"+ {reg} {value |> v2s}"
    | ADDR (reg, other) -> $"+ {reg} {other}"
    | MULI (reg, value) -> $"* {reg} {value |> v2s}"
    | MULR (reg, other) -> $"* {reg} {other}"
    | DIVI (reg, i) -> $"/ {reg} {i}"
    | MODI (reg, i) -> $"%% {reg} {i}"
    | EQLI (reg, value) -> $"＝ {reg} {value |> v2s}"
    | EQLR (reg, other) -> $"＝ {reg} {other}"
    | SETI (reg, value) -> $"{reg} := {value |> v2s}"
    | SETR (reg, other) -> $"{reg} := {other}"
    | _ -> inst |> string
    |> printfn "%s"

let printALU (alu:ALU) =
    printfn "ALU:"
    printfn $"  W: {alu.TryFind W |> Option.get |> v2s}"
    printfn $"  X: {alu.TryFind X |> Option.get |> v2s}"
    printfn $"  Y: {alu.TryFind Y |> Option.get |> v2s}"
    printfn $"  Z: {alu.TryFind Z |> Option.get |> v2s}"

let printProgram (program: Program) =
    program |> List.map printInstruction


let withInputs (program: Program) =
    let rec eval (program: Program) (i: int) =
        match program with
        | [] -> []
        | inst :: rest ->
            match inst with
            | INP reg ->
                let inst = SETI(reg, ANY_INPUT i)
                let i = i + 1
                inst :: (eval rest i)
            | _ -> inst :: (eval rest i)

    eval program 0

let input = file |> List.map parseLine

let program = input |> withInputs

// printProgram program

let smallest (value: Value) =
    match value with
    | CONST i -> i
    | INPUT (_, v) -> v |> Map.toList |> List.map snd |> List.min
    | MULTIPLE s -> s.MinimumElement
    | RANGE (min, max) -> min

let largest (value: Value) =
    match value with
    | CONST i -> i
    | INPUT (_, v) -> v |> Map.toList |> List.map snd |> List.max
    | MULTIPLE s -> s.MaximumElement
    | RANGE (min, max) -> max


let rec addValue (v1: Value) (v2: Value) =
    match (v1, v2) with
    | CONST v1, CONST v2 -> CONST(v1 + v2)
    | _, CONST _ -> addValue v2 v1
    | CONST v1, INPUT (i, vals) -> INPUT(i, vals |> Map.map (fun _ value -> value + v1))
    | CONST v1, MULTIPLE s -> MULTIPLE(s |> Set.map ((+) v1))
    | CONST v1, RANGE (min, max) -> RANGE(min + v1, max + v1)
    | MULTIPLE m1, MULTIPLE m2 when m1.Count * m2.Count < 20 ->
        let m1 = m1 |> Set.toList
        let m2 = m2 |> Set.toList

        List.allPairs m1 m2
        |> List.map (fun (a, b) -> a + b)
        |> Set
        |> MULTIPLE
    | v1, v2 -> RANGE(smallest v1 + smallest v2, largest v1 + largest v2)

let rec mulValue (v1: Value) (v2: Value) =
    match (v1, v2) with
    | CONST v1, CONST v2 -> CONST(v1 * v2)
    | _, CONST _ -> mulValue v2 v1
    | CONST v1, INPUT (i, vals) -> INPUT(i, vals |> Map.map (fun _ value -> value * v1))
    | CONST v1, MULTIPLE s -> MULTIPLE(s |> Set.map ((*) v1))
    | CONST v1, RANGE (min, max) when v1 >= 0 -> RANGE(min * v1, max * v1)
    | CONST v1, RANGE (min, max) when v1 <= 0 -> RANGE(max * v1, min * v1)
    | MULTIPLE m1, MULTIPLE m2 when m1.Count * m2.Count < 20 ->
        let m1 = m1 |> Set.toList
        let m2 = m2 |> Set.toList

        List.allPairs m1 m2
        |> List.map (fun (a, b) -> a * b)
        |> Set
        |> MULTIPLE
    | v1, v2 ->
        let min1 = smallest v1
        let min2 = smallest v2
        let max1 = largest v1
        let max2 = largest v2

        let all =
            [ (min1 * min2)
              (min1 * max2)
              (max1 * min2)
              (max1 * max2) ]

        let min = all |> List.min
        let max = all |> List.max
        RANGE(min, max)

let divValue (value: Value) (v2: int64) =
    match value with
    | CONST i -> CONST(i / v2)
    | INPUT (i, vals) -> INPUT(i, vals |> Map.map (fun _ value -> value / v2))
    | MULTIPLE m -> m |> Set.map (fun v -> v / v2) |> MULTIPLE
    | RANGE (min, max) -> RANGE(min / v2, max / v2)

let modValue (value: Value) (v2: int64) =
    match value with
    | CONST i -> CONST(i % v2)
    | INPUT (i, vals) -> INPUT(i, vals |> Map.map (fun _ value -> value % v2))
    | MULTIPLE m -> m |> Set.map (fun v -> v % v2) |> MULTIPLE
    | RANGE (min, max) -> RANGE(0,v2)

let rec eqValue (v1: Value) (v2: Value) =
    let eq a b = if a = b then 1L else 0L
    let BOOLS = [0L;1L] |> Set |> MULTIPLE

    match v1, v2 with
    | CONST v1, CONST v2 -> CONST(eq v1 v2)
    | _, CONST _ -> eqValue v2 v1
    | CONST v1, INPUT (i, vals) ->
        let vals = vals |> Map.map (fun _ value -> eq value v1)
        let alts = vals |> Map.toList |> List.map snd |> Set |> Set.toList
        if alts.Length = 1 then
            CONST alts.Head
        else
            INPUT (i, vals)
    | CONST v1, MULTIPLE m -> m |> Set.map (eq v1) |> MULTIPLE
    | CONST v1, RANGE (min, max) when v1 > max || v1 < min -> CONST 0L
    | CONST v1, RANGE _ -> BOOLS
    | INPUT (i,vals), INPUT(_,v2) ->
        let v1 = vals |> Map.toList |> List.map snd
        let v2 = v2 |> Map.toList |> List.map snd
        let possibles = List.allPairs v1 v2
                        |> List.map (fun (a,b) -> eq a b)
                        |> Set
        if possibles = Set.singleton(i) then
            CONST i
        else
            BOOLS
    | INPUT (i,vals),MULTIPLE m ->
        let v = vals |> Map.toList |> List.map snd
        let v = v |> List.map m.Contains |> Set
        if v = Set.singleton (false) then
            INPUT (i, vals |> Map.map (fun _ v -> 0L))
        else
            BOOLS
    | MULTIPLE _, INPUT _ -> eqValue v2 v1
    | v1,v2 ->
        let min1 = smallest v1
        let min2 = smallest v2
        let max1 = largest v1
        let max2 = largest v2
        let overlaps = max1 >= min2 && min1 <= max2
        if overlaps then BOOLS else CONST 0L

let isDetermined (value:Value) =
    match value with
    | CONST _ -> true
    | INPUT _ -> true
    | RANGE _ -> false
    | MULTIPLE _ -> false 

let constElim (program: Program) =
    let rec eval (alu: Map<Reg, Value>) (program: Program) =
        let known = alu.ContainsKey
        let get = alu.TryFind >> Option.get
        let isZero = alu.TryFind >> ((=) (Some(CONST 0L)))
        let isOne = alu.TryFind >> ((=) (Some(CONST 1L)))
        
        let skip inst rest =
            printfn $"Skipping: {inst}"
            eval alu rest

        match program with
        | [] -> []
        | inst :: rest ->
            printfn "--"
            printALU alu 
            printInstruction inst 
            match inst with
            | INP _ -> failwith $"Not supported: ${INP}"
            | ADDI (reg, i) when known reg ->
                let value = addValue (get reg) i
                let alu = alu.Add(reg, value)
                let inst = if isDetermined value then SETI(reg, value) else inst
                inst :: (eval alu rest)
            | ADDR (_, r2) when isZero r2 ->
                skip inst rest
            | ADDR (r1, r2) when known r1 && known r2 ->
                let value = addValue (get r1) (get r2)
                let alu = alu.Add(r1, value)
                let inst = if isDetermined value then SETI(r1, value) else inst
                inst :: (eval alu rest)
            | MULI (reg, _) when isZero reg -> 
                skip inst rest
            | MULI (reg, CONST 0L) ->
                let value = CONST 0L
                let alu = alu.Add(reg,value)
                let inst = SETI (reg,value)
                inst :: (eval alu rest)
            | MULI (reg, i) when known reg ->
                let value = mulValue (get reg) i
                let alu = alu.Add(reg, value)
                let inst = if isDetermined value then SETI(reg, value) else inst
                inst :: (eval alu rest)
            | MULR (r1,_) when isZero r1 ->
                skip inst rest
            | MULR (_,r2) when isOne r2 ->
                skip inst rest
            | MULR (r1, r2) when isZero r2 ->
                let value = CONST 0L
                let alu = alu.Add(r1, value)
                let inst = SETI (r1,value)
                inst :: (eval alu rest)
            | MULR (r1, r2) when known r1 && known r2 ->
                let value = mulValue (get r1) (get r2)
                let alu = alu.Add(r1, value)
                let inst = if isDetermined value then SETI(r1, value) else inst
                inst :: (eval alu rest)
            | DIVI (_, 1L) ->
                skip inst rest
            | DIVI (r1, 1L) when isZero r1 ->
                skip inst rest
            | DIVI (r1, i) when known r1 ->
                let value = divValue (get r1) i
                let alu = alu.Add(r1, value)
                let inst = if isDetermined value then SETI(r1, value) else inst
                inst :: (eval alu rest)
            | MODI (r1, _) when isZero r1 ->
                skip inst rest
            | MODI (r1, _) when isOne r1 ->
                skip inst rest
            | MODI (r1, i) when known r1 ->
                if (largest (get r1) < i) then
                    skip inst rest
                else 
                    let value = modValue (get r1) i
                    let alu = alu.Add(r1, value)
                    let inst = if isDetermined value then SETI(r1, value) else inst
                    inst :: (eval alu rest)
            | EQLI (r1, i) when known r1 ->
                let value = eqValue (get r1) i
                let alu = alu.Add (r1, value)
                let inst = if isDetermined value then SETI (r1,value) else inst
                inst :: (eval alu rest)
            | EQLR (r1,r2) when known r1 && known r2 ->
                let value = eqValue (get r1) (get r2)
                let alu = alu.Add (r1,value)
                let inst = if isDetermined value then SETI(r1, value) else inst
                inst :: (eval alu rest)
            | SETI (r1, i) ->
                let alu = alu.Add(r1, i)
                inst :: (eval alu rest)
            | _ ->
                printfn $"NOT IMPLEMENTED: ${inst}"
                inst :: (eval alu rest)

    let initALU = fillALU (CONST 0L)
    eval initALU program

let program1 = constElim program

printfn "CONST ELIMINATED: "
printProgram program1 
