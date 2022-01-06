open System
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

let _0_or_1 = [ 0L; 1L ] |> Set |> MULTIPLE
let _1 = CONST 1L
let _0 = CONST 0L
let ANY_POSITIVE_NZ = RANGE(1, Int64.MaxValue / 1000L)
let ANY_POSITIVE = RANGE(0, Int64.MaxValue / 1000L)
let ANY_NEGATIVE = RANGE(Int64.MinValue / 1000L, -1L)

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

type ALU = Map<Reg, Value>

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
    | _ -> failwith $"Unrecognized register: {s}"

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
            |> List.map vals.TryFind
            |> List.map (Option.map string)
            |> List.map (Option.defaultValue "_")
            |> String.concat " "

        $"{i}#{vals}"
    | MULTIPLE s ->
        s
        |> Set.toList
        |> List.map string
        |> String.concat " "
        |> sprintf "{ %s }"
    | RANGE (a, b) -> $"[{a} .. {b} ]"
    | INVALID -> "INVALID"

let printInstruction (inst: Inst) =
    match inst with
    | INP reg -> $"{reg} ⬅"
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
    |> printfn "%s"

let printALU (alu: ALU) =
    let que = "?"
    printf "ALU:"

    printf
        $"  W: {alu.TryFind W
                |> Option.map v2s
                |> Option.defaultValue que}"

    printf
        $"  X: {alu.TryFind X
                |> Option.map v2s
                |> Option.defaultValue que}"

    printf
        $"  Y: {alu.TryFind Y
                |> Option.map v2s
                |> Option.defaultValue que}"

    printfn
        $"  Z: {alu.TryFind Z
                |> Option.map v2s
                |> Option.defaultValue que}"

let printProgram (program: Program) =
    program |> List.map printInstruction |> ignore


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

let isLimitedInput (value: Value) =
    match value with
    | INPUT (_, vals) -> vals.Count < 9
    | _ -> false

let smallest (value: Value) =
    match value with
    | CONST i -> i
    | INPUT (_, v) -> v |> Map.toList |> List.map snd |> List.min
    | MULTIPLE s -> s.MinimumElement
    | RANGE (min, _) -> min
    | other -> failwith $"Not implemented: {other}"

let largest (value: Value) =
    match value with
    | CONST i -> i
    | INPUT (_, v) -> v |> Map.toList |> List.map snd |> List.max
    | MULTIPLE s -> s.MaximumElement
    | RANGE (_, max) -> max
    | other -> failwith $"Not implemented: ${other}"


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
    | INVALID -> INVALID

let modValue (value: Value) (v2: int64) =
    match value with
    | CONST i -> CONST(i % v2)
    | INPUT (i, vals) -> INPUT(i, vals |> Map.map (fun _ value -> value % v2))
    | MULTIPLE m -> m |> Set.map (fun v -> v % v2) |> MULTIPLE
    | RANGE (_, max) when max < v2 -> value
    | RANGE _ -> RANGE(0, v2 - 1L)
    | INVALID -> INVALID

let rec eqValue (v1: Value) (v2: Value) =
    let eq a b = if a = b then 1L else 0L
    let BOOLS = [ 0L; 1L ] |> Set |> MULTIPLE

    match v1, v2 with
    | CONST v1, CONST v2 -> CONST(eq v1 v2)
    | _, CONST _ -> eqValue v2 v1
    | CONST v1, INPUT (i, vals) ->
        let vals =
            vals |> Map.map (fun _ value -> eq value v1)

        let alts =
            vals
            |> Map.toList
            |> List.map snd
            |> Set
            |> Set.toList

        if alts.Length = 1 then
            CONST alts.Head
        else
            INPUT(i, vals)
    | CONST v1, MULTIPLE m -> m |> Set.map (eq v1) |> MULTIPLE
    | CONST v1, RANGE (min, max) when v1 > max || v1 < min -> CONST 0L
    | CONST _, RANGE _ -> BOOLS
    | INPUT (i, vals), INPUT (_, v2) ->
        let v1 = vals |> Map.toList |> List.map snd
        let v2 = v2 |> Map.toList |> List.map snd

        let possibles =
            List.allPairs v1 v2
            |> List.map (fun (a, b) -> eq a b)
            |> Set

        if possibles = Set.singleton (i) then
            CONST i
        else
            BOOLS
    | INPUT (i, vals), MULTIPLE m ->
        let v = vals |> Map.toList |> List.map snd
        let v = v |> List.map m.Contains |> Set

        if v = Set.singleton false then
            INPUT(i, vals |> Map.map (fun _ _ -> 0L))
        else
            BOOLS
    | MULTIPLE _, INPUT _ -> eqValue v2 v1
    | v1, v2 ->
        let min1 = smallest v1
        let min2 = smallest v2
        let max1 = largest v1
        let max2 = largest v2
        let overlaps = max1 >= min2 && min1 <= max2
        if overlaps then BOOLS else CONST 0L

let isDetermined (value: Value) =
    match value with
    | CONST _ -> true
    | INPUT _ -> true
    | RANGE _ -> false
    | MULTIPLE _ -> false
    | INVALID -> false

let constElim (program: Program) =
    let rec eval (alu: Map<Reg, Value>) (program: Program) =
        let known = alu.ContainsKey
        let get = alu.TryFind >> Option.get
        let isZero = alu.TryFind >> ((=) (Some(CONST 0L)))
        let isOne = alu.TryFind >> ((=) (Some(CONST 1L)))

        let skip inst rest = eval alu rest

        match program with
        | [] -> []
        | inst :: rest ->
            match inst with
            | INP _ -> failwith $"Not supported: ${INP}"
            | ADDI (reg, i) when known reg ->
                let value = addValue (get reg) i
                let alu = alu.Add(reg, value)

                let inst =
                    if isDetermined value then
                        SETI(reg, value)
                    else
                        inst

                inst :: (eval alu rest)
            | ADDR (_, r2) when isZero r2 -> skip inst rest
            | ADDR (r1, r2) when known r1 && known r2 ->
                let value = addValue (get r1) (get r2)
                let alu = alu.Add(r1, value)

                let inst =
                    if isDetermined value then
                        SETI(r1, value)
                    else
                        inst

                inst :: (eval alu rest)
            | MULI (reg, _) when isZero reg -> skip inst rest
            | MULI (reg, CONST 0L) ->
                let value = CONST 0L
                let alu = alu.Add(reg, value)
                let inst = SETI(reg, value)
                inst :: (eval alu rest)
            | MULI (reg, i) when known reg ->
                let value = mulValue (get reg) i
                let alu = alu.Add(reg, value)

                let inst =
                    if isDetermined value then
                        SETI(reg, value)
                    else
                        inst

                inst :: (eval alu rest)
            | MULR (r1, _) when isZero r1 -> skip inst rest
            | MULR (_, r2) when isOne r2 -> skip inst rest
            | MULR (r1, r2) when isZero r2 ->
                let value = CONST 0L
                let alu = alu.Add(r1, value)
                let inst = SETI(r1, value)
                inst :: (eval alu rest)
            | MULR (r1, r2) when known r1 && known r2 ->
                let value = mulValue (get r1) (get r2)
                let alu = alu.Add(r1, value)

                let inst =
                    if isDetermined value then
                        SETI(r1, value)
                    else
                        inst

                inst :: (eval alu rest)
            | DIVI (_, 1L) -> skip inst rest
            | DIVI (r1, 1L) when isZero r1 -> skip inst rest
            | DIVI (r1, i) when known r1 ->
                let value = divValue (get r1) i
                let alu = alu.Add(r1, value)

                let inst =
                    if isDetermined value then
                        SETI(r1, value)
                    else
                        inst

                inst :: (eval alu rest)
            | MODI (r1, _) when isZero r1 -> skip inst rest
            | MODI (r1, _) when isOne r1 -> skip inst rest
            | MODI (r1, i) when known r1 ->
                if (largest (get r1) < i) then
                    skip inst rest
                else
                    let value = modValue (get r1) i
                    let alu = alu.Add(r1, value)

                    let inst =
                        if isDetermined value then
                            SETI(r1, value)
                        else
                            inst

                    inst :: (eval alu rest)
            | EQLI (r1, i) when known r1 ->
                let value = eqValue (get r1) i
                let alu = alu.Add(r1, value)

                let inst =
                    if isDetermined value then
                        SETI(r1, value)
                    else
                        inst

                inst :: (eval alu rest)
            | EQLR (r1, r2) when known r1 && known r2 ->
                let value = eqValue (get r1) (get r2)
                let alu = alu.Add(r1, value)

                let inst =
                    if isDetermined value then
                        SETI(r1, value)
                    else
                        inst

                inst :: (eval alu rest)
            | SETI (r1, i) ->
                let alu = alu.Add(r1, i)
                inst :: (eval alu rest)
            | _ -> failwith $"Not implemented ${inst}"

    let initALU = fillALU (CONST 0L)
    eval initALU program

let program1 = constElim program

let removeUnused (program: Program) =
    let rec eval (inUse: Set<Reg>) (program: Program) =
        let used = inUse.Contains

        match program with
        | [] -> []
        | inst :: rest ->
            let skip () = eval inUse rest
            let cont () = inst :: (eval inUse rest)

            let continueWith (reg: Reg) =
                let inUse = inUse.Add reg
                inst :: (eval inUse rest)

            let continueWithout (reg: Reg) =
                let inUse = inUse.Remove(reg)
                inst :: (eval inUse rest)

            match inst with
            | INP _ -> failwith "INP is not supported"
            | ADDI (reg, _) when used reg -> cont ()
            | ADDI _ -> skip ()
            | ADDR (r1, r2) when used r1 -> continueWith r2
            | ADDR _ -> skip ()
            | MULI (reg, _) when used reg -> cont ()
            | MULR (r1, r2) when used r1 -> continueWith r2
            | DIVI (reg, _) when used reg -> cont ()
            | DIVI _ -> skip ()
            | MODI (reg, _) when used reg -> cont ()
            | MODI _ -> skip ()
            | EQLI (r1, _) when used r1 -> cont ()
            | EQLI _ -> skip ()
            | EQLR (r1, r2) when used r1 -> continueWith r2
            | EQLR _ -> skip ()
            | SETI (r1, _) when used r1 -> continueWithout r1
            | SETI _ -> skip ()
            | SETR (r1, r2) when used r1 ->
                let inUse = inUse.Remove r1
                let inUse = inUse.Add r2
                inst :: (eval inUse rest)
            | SETR _ -> skip ()
            | _ -> failwith $"Not implemented: {inst}"

    program
    |> List.rev
    |> eval (Set.singleton Z)
    |> List.rev

let program2 = removeUnused program1

printfn "Removed unused code"
printfn "Code elimination effect: {program.Length} -> {program1.Length} -> {program2.Length}"
printfn ""

// OK, now we have a pretty optimal program, only one thing remains... solving the task?
// nah... pushing inputs down to optimize the amount of processing that can be done
// before uncertainty

let rec pushDownInputs (program: Program) =
    match program with
    | [] -> []
    | [ inst ] -> [ inst ]
    | inst :: inst2 :: rest ->
        match inst with
        | SETI (reg, INPUT _) ->
            let shuffle =
                match inst2 with
                | ADDI (r2, _) when r2 = reg -> false
                | ADDI _ -> true
                | ADDR (r2, r3) when r2 = reg || r3 = reg -> false
                | ADDR _ -> true
                | MULI (r2, _) when r2 = reg -> false
                | MULI _ -> true
                | MULR (r2, r3) when r2 = reg || r3 = reg -> false
                | MULR _ -> true
                | DIVI (r2, _) when r2 = reg -> false
                | DIVI _ -> true
                | MODI (r2, _) when r2 = reg -> false
                | MODI _ -> true
                | EQLI (r2, _) when r2 = reg -> false
                | EQLI _ -> true
                | EQLR (r2, r3) when r2 = reg || r3 = reg -> false
                | EQLR _ -> true
                | SETI (_, INPUT _) -> false
                | SETI (r2, _) when r2 = reg -> failwith $"leftover shadowing: {inst2} shadows {inst}"
                | SETI _ -> true
                | SETR (r2, r3) when r2 = reg || r3 = reg ->
                    failwith $"Direct assignment of known value: {r2}<-{r3} following {inst}"
                | SETR _ -> true
                | _ -> failwith $"Not implemented: {inst2}"

            if shuffle then
                inst2 :: (pushDownInputs (inst :: rest))
            else
                inst :: (pushDownInputs (inst2 :: rest))
        | _ -> inst :: (pushDownInputs (inst2 :: rest))

let program3 = pushDownInputs program2

printfn "Pushed down inputs"

printProgram program3

let canContain (value: Value) (i: int64) =
    match value with
    | CONST c -> c = i
    | INPUT (_, vals) -> vals |> Map.toList |> List.exists (snd >> (=) i)
    | MULTIPLE s -> s.Contains i
    | RANGE (min, max) -> min <= i && max >= i
    | INVALID -> false

let isOdd (i: int64) = i &&& 1L = 1L
let isEven (i: int64) = i &&& 1L = 0L

let isOddValue (value: Value) =
    match value with
    | CONST i -> isOdd i
    | INPUT (_, vals) -> vals.Values |> Seq.map isOdd |> Seq.contains true
    | MULTIPLE s -> s |> Set.map isOdd |> ((=) (Set.singleton false))
    | RANGE (min, max) -> min <> max
    | INVALID -> false

let isEvenValue (value: Value) =
    match value with
    | CONST i -> isEven i
    | INPUT (_, vals) -> vals.Values |> Seq.map isEven |> Seq.contains true
    | MULTIPLE s -> s |> Set.map isEven |> ((=) (Set.singleton false))
    | RANGE (min, max) -> min <> max
    | INVALID -> false

let filterValue (value: Value) (f: int64 -> bool) =
    match value with
    | CONST i -> if f i then CONST i else INVALID
    | INPUT (i, vals) ->
        let vals = vals |> Map.filter (fun _ v -> f v)
        INPUT(i, vals)
    | MULTIPLE s -> s |> Set.filter f |> MULTIPLE
    | RANGE (min, max) -> RANGE(min, max) // this could be better
    | INVALID -> INVALID

let rec consolidateLossy (value: Value) =
    match value with
    | CONST c -> CONST c
    | INPUT (_, vals) -> vals.Values |> Set |> MULTIPLE |> consolidateLossy
    | MULTIPLE s when s.Count = 0 -> INVALID
    | MULTIPLE s when s.Count = 1 -> s |> Set.toList |> List.head |> CONST
    | MULTIPLE _ -> value
    | RANGE (min, max) when min = max -> CONST min
    | RANGE (min, max) when min + 20L > max -> { min .. max } |> Set |> MULTIPLE
    | RANGE _ -> value
    | INVALID -> INVALID


type Constraint =
    | C_ZERO of Reg
    | C_EQ of Reg * int64
    | C_GT of Reg * int64
    | C_LT of Reg * int64
    | C_ODD of Reg
    | C_EVEN of Reg
    | C_AND of List<Constraint>
    | C_OR of List<Constraint>
    | C_SUCCESS
    | C_FAIL
    | C_MOD_BY of Reg * int64 * int64 // reg,mod,rest
    | C_REQ_INPUT of Value

let bothPositive (r1: Reg) (r2: Reg) = C_AND [ C_GT(r1, 0L); C_GT(r2, 0L) ]
let bothNegative (r1: Reg) (r2: Reg) = C_AND [ C_LT(r1, 0L); C_GT(r2, 0L) ]
let bothZero (r1: Reg) (r2: Reg) = C_AND [ C_ZERO r1; C_ZERO r2 ]

let sameSignNZ (r1: Reg) (r2: Reg) =
    C_OR [ bothPositive r1 r2
           bothNegative r1 r2 ]

let oppositeSignNZ (r1: Reg) (r2: Reg) =
    C_OR [ C_AND [ C_GT(r1, 0L); C_LT(r2, 0L) ]
           C_AND [ C_LT(r1, 0L); C_GT(r2, 0L) ] ]

let bothEven (r1: Reg) (r2: Reg) = C_AND [ C_EVEN r1; C_EVEN r2 ]
let bothOdd (r1: Reg) (r2: Reg) = C_AND [ C_ODD r1; C_ODD r2 ]

let rec cFindR (reg: Reg) (con: Constraint) : Constraint =
    match con with
    | C_ZERO r1 when r1 = reg -> con
    | C_GT (r1, _) when r1 = reg -> con
    | C_GT (r1, _) when r1 = reg -> con
    | C_ODD r1 when r1 = reg -> con
    | C_EVEN r1 when r1 = reg -> con
    | C_EQ (r1, _) when r1 = reg -> con
    | C_AND cs -> cs |> List.map (cFindR reg) |> C_AND
    | C_OR cs -> cs |> List.map (cFindR reg) |> C_OR
    | _ -> C_SUCCESS

let rec cApplyAddr (r1: Reg) (r2: Reg) (con: Constraint) : Constraint =
    let notImpl () =
        failwith $"Not implemented: {r1} {r2} {con}"

    match con with
    | C_OR ors -> ors |> List.map (cApplyAddr r1 r2) |> C_OR
    | C_AND ands -> ands |> List.map (cApplyAddr r1 r2) |> C_AND
    | C_ZERO r when r = r1 ->
        let bothZero = C_AND [ C_ZERO r1; C_ZERO r2 ]
        let opposites = oppositeSignNZ r1 r2
        let values = C_OR [ bothZero; opposites ]
        let odds = bothOdd r1 r2
        let evens = bothEven r1 r2
        let oddEvens = C_OR [ odds; evens ]
        C_AND [ values; oddEvens ]
    | C_ZERO _ -> con
    | C_EQ (r, i) when i = 0L -> cApplyAddr r1 r2 (C_ZERO r)
    | C_EQ (r, _) when r = r1 -> notImpl ()
    | C_EQ _ -> con
    | C_LT (r, i) when r = r1 ->
        C_OR [ C_LT(r1, i + 1L)
               C_LT(r2, i + 1L) ]
    | C_LT _ -> con
    | C_GT (r, i) when r = r1 && i >= 0L ->
        let onePositive = C_OR [ C_GT(r1, 0L); C_GT(r2, 0L) ]

        let summedBySmaller =
            C_OR [ C_LT(r1, i + 1L)
                   C_LT(r2, i + 1L) ]

        C_AND [ onePositive; summedBySmaller ]
    | C_GT (r, i) when r = r1 && i < 0L ->
        C_OR [ C_LT(r1, i + 1L)
               C_LT(r2, i + 1L) ]
    | C_GT _ -> con
    | C_EVEN r when r = r1 ->
        let even = bothEven r1 r2
        let odd = bothOdd r1 r2
        C_OR [ even; odd ]
    | C_EVEN _ -> con
    | C_ODD r when r = r1 ->
        let oddEven = C_AND [ C_ODD r1; C_EVEN r2 ]
        let evenOdd = C_AND [ C_EVEN r1; C_ODD r2 ]
        C_OR [ oddEven; evenOdd ]
    | C_ODD _ -> con
    | C_FAIL -> C_FAIL
    | C_SUCCESS -> C_SUCCESS
    | C_REQ_INPUT _ -> con
    | C_MOD_BY (r, mv, mr) when r = r1 ->
        let r2zero = C_AND [ C_MOD_BY(r, mv, mr); C_ZERO r2 ]

        C_OR [ r2zero
               C_LT(r2, 0L)
               C_GT(r2, 0L) ]
    | _ -> notImpl ()

let rec cApplyAddi (reg: Reg) (value: Value) (con: Constraint) : Constraint =
    let allEven = value |> isOddValue |> not
    let allOdd = value |> isEvenValue |> not

    match con, value with
    | C_OR ors, _ -> ors |> List.map (cApplyAddi reg value) |> C_OR
    | C_AND ands, _ -> ands |> List.map (cApplyAddi reg value) |> C_AND
    | C_ZERO r, CONST c when r = reg -> C_EQ(r, -c)
    | C_ZERO r, _ when r = reg -> failwith $"Not implemented:cApplyAddi {con}"
    | C_ZERO _, _ -> con
    | C_GT (r, i), value when r = reg ->
        match value with
        | CONST v -> C_GT(r, i - v)
        | _ -> failwith $"Not implemented:{con}"
    | C_GT _, _ -> con
    | C_LT (r, i), value when r = reg ->
        match value with
        | CONST v -> C_GT(r, i + v)
        | _ -> failwith $"Not implemented:{con}"
    | C_GT _, _ -> con

    | C_GT _, _ -> con
    | C_LT (r, _), _ when r = reg -> failwith $"Not implemented:cApplyAddi {con}"
    | C_LT _, _ -> con
    | C_ODD r, _ when r = reg && allEven -> con
    | C_ODD r, _ when r = reg && allOdd -> C_EVEN r
    | C_ODD _, _ -> con
    | C_EVEN r, _ when r = reg && allEven -> con
    | C_EVEN r, _ when r = reg && allOdd -> C_ODD r
    | C_EVEN _, _ -> con
    | C_FAIL, _ -> C_FAIL
    | C_SUCCESS, _ -> C_SUCCESS
    | C_REQ_INPUT _, _ -> con
    | _ -> failwith $"Not implemented:cApplyAddi {con}"

let rec cApplyMulr (r1: Reg) (r2: Reg) (con: Constraint) : Constraint =
    match con with
    | C_OR ors -> ors |> List.map (cApplyMulr r1 r2) |> C_OR
    | C_AND ands -> ands |> List.map (cApplyMulr r1 r2) |> C_AND
    | C_ZERO r when r1 = r -> C_OR [ C_ZERO r1; C_ZERO r2 ]
    | C_ZERO _ -> con
    | C_EQ (r, i) when r1 = r && i = 0L -> cApplyMulr r1 r2 (C_ZERO r)
    | C_EQ (r, i) when r1 = r && i < 0L ->
        let range = oppositeSignNZ r1 r2

        let oddEven =
            if isOdd i then
                bothOdd r1 r2
            else
                C_OR [ C_EVEN r1; C_EVEN r2 ]

        C_AND [ range; oddEven ]
    | C_EQ (r, i) when r1 = r && i > 0L ->
        let range = sameSignNZ r1 r2

        let oddEven =
            if isOdd i then
                bothOdd r1 r2
            else
                C_OR [ C_EVEN r1; C_EVEN r2 ]

        C_AND [ range; oddEven ]
    | C_EQ _ -> con
    | C_GT (r, i) when r = r1 && i >= 0L -> sameSignNZ r1 r2
    | C_GT _ -> con
    | C_LT (r, i) when r = r1 && i <= 0L -> oppositeSignNZ r1 r2
    | C_LT _ -> con
    | C_ODD r when r = r1 -> bothOdd r1 r2
    | C_ODD _ -> con
    | C_EVEN r when r = r1 -> C_OR [ C_EVEN r1; C_EVEN r2 ]
    | C_EVEN _ -> con
    | C_FAIL -> C_FAIL
    | C_SUCCESS -> C_SUCCESS
    | C_REQ_INPUT _ -> con
    | _ -> failwith $"Not implemented: {con}"

let rec cApplySeti (reg: Reg) (value: Value) (con: Constraint) =
    match con with
    | C_AND ands -> ands |> List.map (cApplySeti reg value) |> C_AND
    | C_OR ors -> ors |> List.map (cApplySeti reg value) |> C_OR
    | C_ZERO r when r = reg ->
        if canContain value 0L then
            match value with
            | INPUT (i, vals) ->
                let input =
                    INPUT(i, vals |> Map.filter (fun _ v -> v = 0L))

                C_REQ_INPUT input
            | _ -> C_SUCCESS
        else
            C_FAIL
    | C_ZERO _ -> con
    | C_GT (r, i) when r = reg ->
        if largest value > i then
            match value with
            | INPUT (index, vals) ->
                let input =
                    INPUT(index, vals |> Map.filter (fun _ v -> v > i))

                C_REQ_INPUT input
            | _ -> C_SUCCESS
        else
            C_FAIL
    | C_GT _ -> con
    | C_LT (r, i) when r = reg ->
        if smallest value < i then
            C_SUCCESS
        else
            C_FAIL
    | C_LT _ -> con
    | C_ODD r when r = reg ->
        if isOddValue value then
            C_SUCCESS
        else
            C_FAIL
    | C_ODD _ -> con
    | C_EVEN r when r = reg ->
        if isEvenValue value then
            C_SUCCESS
        else
            C_FAIL
    | C_EVEN _ -> con
    | C_FAIL -> C_FAIL
    | C_SUCCESS -> C_SUCCESS
    | C_REQ_INPUT _ -> con
    | C_MOD_BY (r, mv, mr) when r = reg ->
        let value = modValue value mv

        if canContain value mr then
            match value with
            | CONST c -> C_SUCCESS
            | INPUT _ -> filterValue value ((=) mr) |> C_REQ_INPUT
            | _ -> C_SUCCESS
        else
            C_FAIL
    | C_MOD_BY _ -> con
    | _ -> failwith $"Not implemented: {con}"

let rec cApplyEqli (reg: Reg) (value: Value) (con: Constraint) =
    match con with
    | C_OR ors -> ors |> List.map (cApplyEqli reg value) |> C_OR
    | C_AND ands -> ands |> List.map (cApplyEqli reg value) |> C_AND

    | C_ZERO r when r = reg && value = (CONST 0L) -> C_OR [ C_GT(r, 0L); C_LT(r, 0L) ]
    | C_ZERO r when r = reg -> C_SUCCESS
    | C_ZERO _ -> con
    | C_EQ (r, 1L) when r = reg ->
        match value with
        | CONST c -> C_EQ(r, c)
        | MULTIPLE s when s.Count < 3 ->
            s
            |> Set.toList
            |> List.map (fun s -> C_EQ(r, s))
            |> C_OR
        | _ ->
            C_AND [ C_LT(r, largest value + 1L)
                    C_GT(r, smallest value - 1L) ]
    | C_EQ (r, 0L) -> cApplyEqli reg value (C_ZERO r)
    | C_GT (r, 1L) when r = reg -> C_FAIL
    | C_GT (r, 0L) when r = reg -> cApplyEqli reg value (C_EQ(r, 1L))
    | C_GT (r, i) when r = reg && i < 0L -> C_SUCCESS
    | C_GT _ -> con
    | C_LT (r, 0L) when r = reg -> C_FAIL
    | C_LT (r, 1L) when r = reg -> cApplyEqli reg value (C_ZERO r)
    | C_LT (r, i) when r = reg && i > 1L -> C_SUCCESS
    | C_LT _ -> con
    | C_ODD r -> cApplyEqli reg value (C_EQ(r, 0L))
    | C_ODD _ -> con
    | C_EVEN r -> cApplyEqli reg value (C_ZERO r)
    | C_EVEN _ -> con
    | C_FAIL -> C_FAIL
    | C_SUCCESS -> C_SUCCESS
    | C_REQ_INPUT _ -> con
    | _ -> failwith $"Not implemented {con}"

let rec cApplyEqlr r1 r2 (con: Constraint) =
    match con with
    | C_OR ors -> ors |> List.map (cApplyEqlr r1 r2) |> C_OR
    | C_AND ands -> ands |> List.map (cApplyEqlr r1 r2) |> C_AND
    | C_ZERO r when r = r1 ->
        let nz1 = C_OR [ C_LT(r1, 0L); C_GT(r1, 0L) ]
        let nz2 = C_OR [ C_LT(r2, 0L); C_GT(r2, 0L) ]
        C_OR [ nz1; nz2 ]
    | C_ZERO _ -> con
    | C_EQ (r, 0L) -> cApplyEqlr r1 r2 (C_ZERO r)
    | C_EQ (r, 1L) when r = r1 ->
        let bothZero = C_AND [ C_ZERO r1; C_ZERO r2 ]
        let above = C_AND [ C_GT(r1, 0L); C_GT(r2, 0L) ]
        let below = C_AND [ C_LT(r1, 0L); C_LT(r2, 0L) ]
        let value = C_OR [ bothZero; above; below ]
        let odd = C_AND [ C_ODD r1; C_ODD r2 ]
        let even = C_AND [ C_EVEN r1; C_EVEN r2 ]
        let bools = C_OR [ odd; even ]
        C_AND [ value; bools ]
    | C_GT (r, i) when r = r1 && i > 0L -> C_FAIL
    | C_GT (r, 0L) when r = r1 -> cApplyEqlr r1 r2 (C_EQ(r, 1L))
    | C_GT (r, i) when r = r1 && i < 1L -> C_SUCCESS
    | C_GT _ -> con
    | C_LT (r, i) when r = r1 && i > 1L -> C_SUCCESS
    | C_LT (r, 1L) when r = r1 -> cApplyEqlr r1 r2 (C_ZERO r)
    | C_LT (r, i) when r = r1 && i < 1L -> C_FAIL
    | C_LT _ -> con
    | C_EVEN r when r = r1 -> cApplyEqlr r1 r2 (C_ZERO r)
    | C_EVEN _ -> con
    | C_ODD r when r = r1 -> cApplyEqlr r1 r2 (C_EQ(r, 1L))
    | C_ODD _ -> con
    | C_SUCCESS -> C_SUCCESS
    | C_FAIL -> C_FAIL
    | C_REQ_INPUT _ -> con
    | _ -> failwith $"Not implemented {r1} {r2} {con}"

let rec cApplyDivi (reg: Reg) (value: int64) (con: Constraint) =
    match con with
    | C_OR ors -> ors |> List.map (cApplyDivi reg value) |> C_OR
    | C_AND ands -> ands |> List.map (cApplyDivi reg value) |> C_AND
    | C_ZERO _ -> con
    | C_EQ (r, i) when r = reg -> C_EQ(reg, i * value)
    | C_EQ _ -> con
    | C_GT (r, i) when r = reg && value > 0 && isEven value -> C_AND [ C_EVEN r; C_GT(r, i * value) ]
    | C_GT (r, i) when r = reg && value > 0 && isOdd value -> C_GT(r, i * value)
    | C_GT (r, _) when r = reg -> failwith $"Not implemented: {con}"
    | C_GT _ -> con
    | C_LT (r, i) when r = reg && value > 0 && isEven value -> C_AND [ C_EVEN r; C_LT(r, i * value) ]
    | C_GT (r, i) when r = reg && i > 0 && value > 0 && isOdd value -> C_LT(r, i * value)
    | C_LT (r, _) when r = reg -> failwith $"Not implemented: {con}"
    | C_LT _ -> con
    | C_EVEN _ -> con
    | C_ODD r when r = reg && isEven value -> C_FAIL
    | C_ODD _ -> con
    | C_FAIL -> C_FAIL
    | C_SUCCESS -> C_SUCCESS
    | C_REQ_INPUT _ -> con
    | _ -> failwith $"Not implemented {con}"

let rec cApplyModi (reg: Reg) (value: int64) (con: Constraint) =
    match con with
    | C_OR ors -> ors |> List.map (cApplyModi reg value) |> C_OR
    | C_AND ands -> ands |> List.map (cApplyModi reg value) |> C_AND
    | C_ZERO r when r = reg -> C_MOD_BY(r, value, 0L)
    | C_ZERO _ -> con
    | C_EQ (r, i) when r = reg && i < 0 -> C_FAIL
    | C_EQ (r, i) when r = reg && i >= value -> C_FAIL
    | C_EQ (r, i) when r = reg && isEven i && isEven value ->
        let modby = C_MOD_BY(reg, value, i)
        let pos_even = C_AND [ C_EVEN r; C_GT(reg, i - 1L) ]
        C_AND [ modby; pos_even ]
    | C_EQ (r, i) when r = reg && isOdd i ->
        let modby = C_MOD_BY(reg, value, i)
        let positive = C_GT(reg, i - 1L)
        C_AND [ modby; positive ]
    | C_EQ (r, i) when r = reg && isOdd value ->
        let modby = C_MOD_BY(reg, value, i)
        let positive = C_GT(reg, i - 1L)
        C_AND [ modby; positive ]
    | C_EQ (r, _) when r = reg -> failwith $"Not implemented {con}"
    | C_EQ _ -> con
    | C_LT (r, i) when r = reg && i <= 0L -> C_FAIL
    | C_LT (r, i) when r = reg && i = 1L -> cApplyModi reg value (C_EQ(reg, 0L))
    | C_LT (r, i) when r = reg && i > 0L -> con
    | C_LT (r, _) when r = reg -> failwith $"Not implemented {con}"
    | C_LT _ -> con
    | C_GT (r, i) when reg = r && i < 0L ->
        let cz = cApplyModi reg value (C_ZERO r)
        let cg = cApplyModi reg value (C_GT(r, 0L))
        C_OR [ cz; cg ]
    | C_GT (r, i) when reg = r && i > 0L -> con
    | C_GT _ -> con
    | C_ODD r when r = reg && isEven value -> con
    | C_ODD r when r = reg && isOdd value -> C_SUCCESS
    | C_ODD _ -> con
    | C_EVEN r when r = reg && isEven value -> con
    | C_EVEN r when r = reg && isOdd value -> C_SUCCESS
    | C_EVEN _ -> con
    | C_FAIL -> C_FAIL
    | C_SUCCESS -> C_SUCCESS
    | C_REQ_INPUT _ -> con

let rec propagateFailSuccess (con: Constraint) =
    match con with
    | C_FAIL -> C_FAIL
    | C_AND ands when ands |> List.contains C_FAIL -> C_FAIL
    | C_AND ands ->
        let ands = ands |> List.map propagateFailSuccess
        let ands = ands |> List.filter ((<>) C_SUCCESS)

        match ands.Length with
        | 0 -> C_SUCCESS
        | 1 -> ands.Head
        | _ -> C_AND ands
    | C_OR ors when ors |> List.contains C_SUCCESS -> C_SUCCESS
    | C_OR ors ->
        let ors = ors |> List.map propagateFailSuccess
        let ors = ors |> List.filter ((<>) C_FAIL)

        match ors.Length with
        | 0 -> C_FAIL
        | 1 -> ors.Head
        | _ -> C_OR ors
    | _ -> con

let rec testRegOddEven (reg: Reg) (odd: bool) (con: Constraint) : bool =
    let even = not odd

    match con with
    | C_AND ands ->
        ands
        |> List.map (testRegOddEven reg odd)
        |> List.contains false
        |> not
    | C_OR ors ->
        ors
        |> List.map (testRegOddEven reg odd)
        |> List.contains true
    | C_ZERO r when r = reg -> even
    | C_ZERO _ -> true
    | C_EQ (r, i) when r = reg && isOdd i -> odd
    | C_EQ (r, i) when r = reg && isEven i -> even
    | C_EQ _ -> true
    | C_LT _ -> true
    | C_GT _ -> true
    | C_ODD r when r = reg -> odd
    | C_ODD _ -> true
    | C_EVEN r when r = reg -> even
    | C_EVEN _ -> true
    | C_FAIL -> false
    | C_SUCCESS -> true
    | C_REQ_INPUT _ -> true
    | C_MOD_BY (r, mv, rv) when r = reg && isEven mv && odd -> isOdd rv
    | C_MOD_BY (r, mv, rv) when r = reg && isEven mv && even -> isEven rv
    | C_MOD_BY (r, mv, _) when r = reg && isOdd mv -> true
    | C_MOD_BY (r, _, _) when r <> reg -> true

    | _ -> failwith $"Missing {con}"

let rec removeOddEven (reg: Reg) (odd: bool) (con: Constraint) : Constraint =
    // printfn $"removeOddEven {reg} {odd} {con}"
    let even = not odd

    match con with
    | C_AND ands -> ands |> List.map (removeOddEven reg odd) |> C_AND
    | C_OR ors -> ors |> List.map (removeOddEven reg odd) |> C_OR
    | C_ZERO r when r = reg -> if even then C_FAIL else con
    | C_ZERO _ -> con
    | C_EQ (r, i) when r = reg && isEven i && even -> C_FAIL
    | C_EQ (r, i) when r = reg && isOdd i && odd -> C_FAIL
    | C_EQ _ -> con
    | C_GT _ -> con
    | C_LT _ -> con
    | C_ODD r when r = reg -> if odd then C_FAIL else C_SUCCESS
    | C_ODD _ -> con
    | C_EVEN r when r = reg -> if even then C_FAIL else C_SUCCESS
    | C_EVEN _ -> con
    | C_FAIL -> C_FAIL
    | C_SUCCESS -> C_SUCCESS
    | C_REQ_INPUT _ -> con
    | C_MOD_BY (r, mv, mr) when r = reg && isEven mr && isOdd mr = odd -> C_FAIL
    | C_MOD_BY (r, mv, mr) when r = reg -> con
    | C_MOD_BY (r, _, _) -> con
    | _ -> failwith $"Missing {con}"

let testRegOddEvenBoth (r: Reg) (con: Constraint) =
    let oddOK = testRegOddEven r true con
    let evenOK = testRegOddEven r false con

    if (not oddOK && not evenOK) then
        failwith $"impossible! {con}"

    let con =
        if not oddOK then
            let con =
                C_AND [ removeOddEven r true con
                        C_EVEN r ]
                |> propagateFailSuccess

            con
        else
            con

    let con =
        if not evenOK then
            let con =
                C_AND [ removeOddEven r false con
                        C_ODD r ]
                |> propagateFailSuccess

            con
        else
            con

    con

let testAllRegsOddEven (con: Constraint) =
    let con = con |> testRegOddEvenBoth W
    let con = con |> testRegOddEvenBoth X
    let con = con |> testRegOddEvenBoth Y
    let con = con |> testRegOddEvenBoth Z
    con

let rec testZero (reg: Reg) (con: Constraint) =
    match con with
    | C_AND ands ->
        ands
        |> List.map (testZero reg)
        |> List.contains false
        |> not
    | C_OR ors ->
        ors
        |> List.map (testZero reg)
        |> List.contains true
    | C_ZERO _ -> true
    | C_EQ (r, i) when r = reg && i <> 0L -> false
    | C_EQ _ -> true
    | C_GT (r, i) when r = reg && i > -1L -> false
    | C_GT _ -> true
    | C_LT (r, i) when r = reg && i < 1L -> false
    | C_LT _ -> true
    | C_ODD r when r = reg -> false
    | C_ODD _ -> true
    | C_EVEN _ -> true
    | C_FAIL -> false
    | C_SUCCESS -> true
    | C_REQ_INPUT _ -> true
    | C_MOD_BY (r, _, 0L) when r = reg -> true
    | C_MOD_BY (r, _, _) when r = reg -> false
    | C_MOD_BY _ -> true
    | _ -> failwith $"Missing {con}"

let rec testNotZero (reg: Reg) (con: Constraint) =
    match con with
    | C_AND ands ->
        ands
        |> List.map (testNotZero reg)
        |> List.contains false
        |> not
    | C_OR ors ->
        ors
        |> List.map (testNotZero reg)
        |> List.contains true
    | C_ZERO r when r = reg -> false
    | C_ZERO _ -> true
    | C_EQ (r, i) when r = reg -> i <> 0L
    | C_EQ _ -> true
    | C_GT _ -> true
    | C_LT _ -> true
    | C_ODD _ -> true
    | C_EVEN _ -> true
    | C_FAIL -> false
    | C_SUCCESS -> true
    | C_REQ_INPUT _ -> true
    | C_MOD_BY _ -> true
    | _ -> failwith $"Missing {con}"

let rec enforceNotZero (reg: Reg) (con: Constraint) =
    match con with
    | C_AND ands -> ands |> List.map (enforceNotZero reg) |> C_AND
    | C_OR ors -> ors |> List.map (enforceNotZero reg) |> C_OR
    | C_ZERO r when r = reg -> C_FAIL
    | C_ZERO _ -> con
    | C_EQ (r, i) when r = reg && i = 0L -> C_FAIL
    | C_EQ _ -> con
    | C_GT (r, i) when r = reg && i = -1L -> C_GT(r, 0L)
    | C_GT _ -> con
    | C_LT (r, i) when r = reg && i = 1L -> C_LT(r, 0L)
    | C_LT _ -> con
    | C_EVEN _ -> con
    | C_ODD _ -> con
    | C_FAIL -> C_FAIL
    | C_SUCCESS -> C_SUCCESS
    | C_REQ_INPUT _ -> con

let rec enforceZero (reg: Reg) (con: Constraint) =
    match con with
    | C_AND ands -> ands |> List.map (enforceZero reg) |> C_AND
    | C_OR ors -> ors |> List.map (enforceZero reg) |> C_OR
    | C_ZERO r when r = reg -> con
    | C_ZERO _ -> con
    | C_EQ (r, 0L) when r = reg -> C_SUCCESS
    | C_EQ (r, _) when r = reg -> C_FAIL
    | C_EQ _ -> con
    | C_GT (r, i) when r = reg && i < 0L -> C_SUCCESS
    | C_GT (r, _) when r = reg -> C_FAIL
    | C_GT _ -> con
    | C_LT (r, i) when r = reg && i > 0L -> C_SUCCESS
    | C_LT (r, _) when r = reg -> C_FAIL
    | C_LT _ -> con
    | C_EVEN r when r = reg -> C_SUCCESS
    | C_EVEN _ -> con
    | C_ODD r when r = reg -> C_FAIL
    | C_ODD _ -> con
    | C_FAIL -> C_FAIL
    | C_SUCCESS -> C_SUCCESS
    | _ -> failwith $"Not implemented {reg} {con}"

let checkZeroes (con: Constraint) =
    let zw = testZero W con
    let zx = testZero X con
    let zy = testZero Y con
    let zz = testZero Z con

    let con =
        if not zw then
            enforceNotZero Z con
        else
            con

    let con =
        if not zx then
            enforceNotZero X con
        else
            con

    let con =
        if not zy then
            enforceNotZero Y con
        else
            con

    let con =
        if not zz then
            enforceNotZero Z con
        else
            con

    printfn $"testZeroes {zw} {zx} {zy} {zz}"
    con

let testNotZeroes (con: Constraint) =
    let zw = testNotZero W con
    let zx = testNotZero X con
    let zy = testNotZero Y con
    let zz = testNotZero Z con
    let con = if zw then con else enforceZero W con
    let con = if zx then con else enforceZero X con
    let con = if zy then con else enforceZero Y con
    let con = if zz then con else enforceZero Z con
    printfn $"testNotZeroes {zw} {zx} {zy} {zz}"
    con

let rec flatten (con: Constraint) =
    let is_and (c: Constraint) =
        match c with
        | C_AND _ -> true
        | _ -> false

    let is_or (c: Constraint) =
        match c with
        | C_OR _ -> true
        | _ -> false

    match con with
    | C_AND ands ->
        let ands = ands |> List.map flatten
        let subs = ands |> List.filter is_and

        if subs.Length > 0 then
            let rest = ands |> List.filter (is_and >> not)
            let subs = subs |> List.map (fun (C_AND l) -> l)
            let ands = rest :: subs |> List.concat
            C_AND ands
        else
            C_AND ands
    | C_OR ors ->
        let ors = ors |> List.map flatten
        let subs = ors |> List.filter is_or

        if subs.Length > 0 then
            let rest = ors |> List.filter (is_or >> not)
            let subs = subs |> List.map (fun (C_OR l) -> l)
            let ors = rest :: subs |> List.concat
            C_OR ors
        else
            C_OR ors
    | _ -> con

let rec removeIneffectiveInputReqs (con: Constraint) =
    match con with
    | C_AND ands ->
        ands
        |> List.map removeIneffectiveInputReqs
        |> C_AND
    | C_OR ors -> ors |> List.map removeIneffectiveInputReqs |> C_OR
    | C_REQ_INPUT value ->
        if isLimitedInput value then
            con
        else
            C_SUCCESS
    | _ -> con

let purge (con: Constraint) =
    // printfn $"Purging {con}"
    let con = con |> propagateFailSuccess
    // printfn $"Purged 1 {con}"
    let con = con |> testAllRegsOddEven
    // printfn $"Purged 2 {con}"
    let con = checkZeroes con
    let con = testNotZeroes con
    let con = removeIneffectiveInputReqs con
    // let con = flatten con
    // printfn $"Flatten {flatten con}"
    con

let rec testValue (reg: Reg) (value: int64) (con: Constraint) =
    match con with
    | C_AND ands ->
        ands
        |> List.map (testValue reg value)
        |> List.contains false
        |> not
    | C_OR ors ->
        ors
        |> List.map (testValue reg value)
        |> List.contains true
    | C_ZERO r when r = reg -> value = 0L
    | C_EQ (r, i) when r = reg -> value = i
    | C_GT (r, i) when r = reg -> i < value
    | C_LT (r, i) when r = reg -> i > value
    | C_ODD r when r = reg -> isOdd value
    | C_EVEN r when r = reg -> isEven value
    | C_SUCCESS -> true
    | C_FAIL -> false
    | _ -> true


let filterInput (reg: Reg) (input: Value) (con: Constraint) =
    printfn $"FILTERING {reg} {input |> v2s} for {con}"

    match input with
    | INPUT (i, vals) ->
        let vals =
            vals
            |> Map.filter (fun _ value -> testValue reg value con)

        INPUT(i, vals)
    | CONST _ -> input
    | _ -> failwith $"Not implemented: {input}"

let checkConstraints (program: Program) : Program =
    let rec check (iter: int) (con: Constraint) (program: Program) =
        let con = purge con

        match program with
        | [] -> []
        | inst :: rest ->
            printfn $"Checking [{iter}:"
            printInstruction inst
            let cont (con: Constraint) = inst :: (check (iter + 1) con rest)

            match inst with
            | ADDR (r1, r2) ->
                let con = cApplyAddr r1 r2 con
                cont con
            | ADDI (r1, i) ->
                let con = cApplyAddi r1 i con
                cont con
            | MULR (r1, r2) ->
                let con = cApplyMulr r1 r2 con
                cont con
            | SETI (r1, value) ->
                printfn "***************************"
                printfn $"ORIGINAL INPUT: {value |> v2s}"
                printfn $"CON: {con}"
                let value = filterInput r1 value con
                printfn $"FILTERED INPUT: {value |> v2s}"
                printfn "***************************"
                let newCon = cApplySeti r1 value con
                cont newCon
            | EQLI (r, value) ->
                let con = cApplyEqli r value con
                cont con
            | EQLR (r1, r2) ->
                let con = cApplyEqlr r1 r2 con
                cont con
            | DIVI (r1, i) ->
                let con = cApplyDivi r1 i con
                cont con
            | MODI (r1, i) ->
                let con = cApplyModi r1 i con
                cont con
            | _ -> failwith $"Not implemented {inst}"

    program
    |> List.rev
    |> check 0 (C_ZERO Z)
    |> List.rev

let program4 = checkConstraints program3

printProgram program4

// ok, this was useless yet again - lets do a new backwards search


type ALUProgram = list<ALU * Inst>

let toALUProgram (program: Program) : ALUProgram =
    let rec eval (alu: ALU) (program: Program) : ALUProgram =
        let get = alu.TryFind >> Option.get

        match program with
        | [] -> []
        | inst :: rest ->
            match inst with
            | ADDI (r1, i) ->
                let alu = alu.Add(r1, addValue (get r1) i)
                (alu, inst) :: (eval alu rest)
            | ADDR (r1, r2) ->
                let alu = alu.Add(r1, addValue (get r1) (get r2))
                (alu, inst) :: (eval alu rest)
            | MULI (r1, i) ->
                let alu = alu.Add(r1, mulValue (get r1) i)
                (alu, inst) :: (eval alu rest)
            | MULR (r1, r2) ->
                let alu = alu.Add(r1, mulValue (get r1) (get r2))
                (alu, inst) :: (eval alu rest)
            | DIVI (r1, i) ->
                let alu = alu.Add(r1, divValue (get r1) i)
                (alu, inst) :: (eval alu rest)
            | MODI (r1, i) ->
                let alu = alu.Add(r1, modValue (get r1) i)
                (alu, inst) :: (eval alu rest)
            | EQLI (r1, i) ->
                let alu = alu.Add(r1, eqValue (get r1) i)
                (alu, inst) :: (eval alu rest)
            | EQLR (r1, r2) ->
                let alu = alu.Add(r1, eqValue (get r1) (get r2))
                (alu, inst) :: (eval alu rest)
            | SETI (r1, i) ->
                let alu = alu.Add(r1, i)
                (alu, inst) :: (eval alu rest)
            | SETR (r1, r2) ->
                let alu = alu.Add(r1, get r2)
                (alu, inst) :: (eval alu rest)
            | _ -> failwith $"Missing case: {inst}"

    let initALU = fillALU (CONST 0L)
    eval initALU program

let revProgram = program |> toALUProgram
revProgram |> List.map (printfn "%A")


let rec intersectWith (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | INVALID, _ -> INVALID
    | _, INVALID -> INVALID
    | CONST c1, CONST c2 when c1 = c2 -> CONST c1
    | CONST c1, CONST c2 -> INVALID
    | CONST c, v2 when canContain v2 c -> CONST c
    | CONST c, _ -> INVALID
    | INPUT (id, vals), _ ->
        let vals =
            vals |> Map.filter (fun v i -> canContain v2 i)

        INPUT(id, vals)
    | _, INPUT _ -> intersectWith v2 v1 // flips, but keeps the INPUT
    | RANGE _, CONST c2 when canContain v1 c2 -> CONST c2
    | RANGE (min1, max1), RANGE (min2, max2) ->
        let newMin = max min1 min2
        let newMax = min max1 max2

        if newMin > newMax then INVALID
        elif newMin = newMax then CONST newMin
        else RANGE(newMin, newMax)
    | RANGE _, MULTIPLE _ -> intersectWith v2 v1
    | MULTIPLE s, _ -> s |> Set.filter (canContain v2) |> MULTIPLE
    | _ -> failwith $"Not implemented {v1} {v2}"

let intersectALUWith (alu1: ALU) (alu2: ALU) : ALU =
    let intersectReg (r: Reg) =
        let maybeV1 = alu1.TryFind r
        let maybeV2 = alu2.TryFind r

        match maybeV1, maybeV2 with
        | None, None -> None
        | _, None -> maybeV1
        | None, _ -> maybeV2
        | Some (v1), Some (v2) -> intersectWith v1 v2 |> Some

    let regs = [ W; X; Y; Z ]

    let alu =
        regs
        |> List.map (fun r -> r, intersectReg r)
        |> List.filter (snd >> Option.isSome)
        |> List.map (fun (a, Some (b)) -> (a, b))
        |> Map

    printfn $"IntersecALUWith: "
    printALU alu1
    printALU alu2
    printfn "===>"
    printALU alu
    printfn "---"
    alu

let sizeOf (v: Value) : int64 =
    match v with
    | CONST _ -> 1L
    | INPUT (_, vals) -> vals.Count |> int64
    | MULTIPLE s -> s.Count |> int64
    | RANGE (r1, r2) -> r2 - r1 + 1L

let expand (v: Value) : Value =
    match v with
    | RANGE (min, max) -> { min .. max } |> Set |> MULTIPLE
    | _ -> v

let rec filterDivisibleByAssumeValid (v1: Value) (v2: Value) : Value * Value =
    let bothZero = (canContain v1 0L) && (canContain v2 0L)

    match v1, v2 with
    | _, CONST 1L -> v1, v2
    | CONST 0L, _ -> v1, v2
    | CONST _, CONST _ -> v1, v2
    | INPUT (i, vals), CONST c ->
        let vals =
            vals |> Map.filter (fun _ v -> v % c = 0L)

        INPUT(i, vals), CONST c
    | INPUT (i1, vals1), INPUT (i2, vals2) when smallest v1 > 0L && smallest v2 > 0L ->
        let vals1 =
            vals1
            |> Map.filter (fun _ v1 -> vals2 |> Map.exists (fun _ v2 -> v1 % v2 = 0L))

        let vals2 =
            vals2
            |> Map.filter (fun _ v2 -> vals1 |> Map.exists (fun _ v1 -> v1 % v2 = 0L))

        INPUT(i1, vals1), INPUT(i2, vals2)
    | RANGE _, _ when sizeOf v1 < 30 -> filterDivisibleByAssumeValid (expand v1) v2
    | _, RANGE _ when sizeOf v2 < 30 -> filterDivisibleByAssumeValid v1 (expand v2)
    | v1,RANGE (min,max) when max > largest v1  ->
        filterDivisibleByAssumeValid v1 (RANGE (min,largest v1))
    | RANGE (min,max),v2 when min > 0L && smallest v2 > 0L ->
        let factors = [1L..min] |> List.filter (fun i -> min % i = 0L) |> List.rev 
        let valids = factors |> List.filter (canContain v2)
        if valids.IsEmpty then filterDivisibleByAssumeValid (RANGE (min+1L,max)) v2 
        else (RANGE(min/valids.Head,max/(smallest v2)),v2)
    | MULTIPLE s1, MULTIPLE s2 ->
        let s2 = s2.Remove(0L)

        let ss =
            Seq.allPairs (s1 |> Set.toSeq) (s2 |> Set.toSeq)
            |> Seq.filter (fun (a, b) -> a % b = 0L)
            |> Seq.toList

        let ss = if bothZero then (0L, 0L) :: ss else ss
        let v1 = ss |> List.map fst |> Set |> MULTIPLE
        let v2 = ss |> List.map snd |> Set |> MULTIPLE
        v1, v2
    | _ -> failwith $"Not implemented: {v1 |> v2s} {v2 |> v2s}"

let rec revMult (v1: Value) (v2: Value) : Option<Value> =
    match v1, v2 with
    | _, _ when canContain v1 0L && canContain v2 0L -> None
    | _, CONST 1L -> Some(v1) 
    | CONST 0L, _ -> None
    | _, CONST 0L -> Some(INVALID)
    | CONST c1, CONST c2 ->
        if (c1 % c2) = 0L then
            CONST(c1 / c2) |> Some
        else
            Some(INVALID)
    | MULTIPLE s, CONST c ->
        let s = s |> Set.filter (fun i -> i % c = 0L)
        let s = s |> Set.map (fun i -> i / c)
        MULTIPLE s |> Some
    | MULTIPLE s1, MULTIPLE s2 ->
        let s2 = s2.Remove(0L)
        let s1 = s1 |> Set.toSeq
        let s2 = s2 |> Set.toSeq

        let s =
            Seq.allPairs s1 s2
            |> Seq.filter (fun (a, b) -> a % b = 0L)
            |> Seq.map (fun (a, b) -> (a / b))
            |> Set

        if s.IsEmpty then
            Some(INVALID)
        else
            MULTIPLE s |> Some
    | MULTIPLE s1, INPUT (i,vals) ->
        let iv = vals |> Map.toList |> List.map snd
        let valid1 s = iv |> List.exists (fun i -> s % i = 0L)
        let s1 = s1 |> Set.filter valid1
        if s1.IsEmpty then None else Some (MULTIPLE s1) 
    | RANGE _, _ when sizeOf v1 < 30 -> revMult (expand v1) v2
    | _, RANGE _ when sizeOf v2 < 30 -> revMult v1 (expand v2)
    | RANGE (min,max), MULTIPLE s2 when min >= 0L && smallest v2 >= 0L ->
        if s2 |> Set.filter (fun i -> min % i = 0L) |> Set.isEmpty |> not then Some(v1)
        else revMult (RANGE(min+1L,max)) v2 
    | _ -> failwith $"Not implemented {v1} {v2}"

let revModI (value: Value) (i: int64) =
    // TODO: STEPPED_RANGE
    addValue ANY_POSITIVE value

let propagateALUBack (program: ALUProgram) : ALUProgram =
    let rec eval (alu: ALU) (program: ALUProgram) : ALUProgram =
        match program with
        | [] -> []
        | (instALU, inst) :: rest ->
            let alu = intersectALUWith instALU alu
            let get = alu.TryFind >> Option.get
            let known = alu.ContainsKey
            printf "Executing: "
            printInstruction inst
            printALU

            match inst with
            | ADDI (r1, CONST i) ->
                if known r1 then
                    let v1 = get r1
                    let v1 = addValue v1 (CONST -i)
                    let newInst = alu, inst
                    let alu = alu.Add(r1, v1)
                    newInst :: (eval alu rest)
                else
                    (alu, inst) :: (eval alu rest)
            | ADDR (r1, r2) ->
                let v1 = get r1
                let v2 = get r2 |> mulValue (CONST -1)
                let v1 = addValue v1 v2
                printfn $"ADDR: Setting {r1} to {v1}"
                let newInst = alu, inst
                let alu = alu.Add(r1, v1)
                newInst :: (eval alu rest)
            | MULI (r1, v2) when v2 = CONST 0L ->
                let newInst = alu, inst
                let alu = alu.Remove r1
                newInst :: (eval alu rest)
            | MULR (r1, r2) ->
                let v1 = get r1
                let v2 = get r2
                let v1, v2 = filterDivisibleByAssumeValid v1 v2
                let alu = alu.Add(r1, v1).Add(r2, v2)
                let newInst = alu, inst
                printfn $"before: {v1}"
                let o1 = revMult v1 v2
                printfn $"after: {o1}"

                let alu =
                    if o1.IsSome then
                        alu.Add(r1, o1 |> Option.get)
                    else
                        alu.Remove(r1)

                newInst :: (eval alu rest)
            | DIVI (r1, i) ->
                let newInst = alu, inst
                let alu = alu.Add(r1, mulValue (get r1) (CONST i))
                newInst :: (eval alu rest)
            | MODI (r1, i) when known r1 ->
                let v1 = get r1
                printfn $"MODI {r1}={v1} {i}"
                // TODO: filter - but range restricted on first pass
                let newInst = alu, inst
                let alu = alu.Add(r1, revModI v1 i)
                newInst :: (eval alu rest)
            | MODI (r1, i) when not (known r1) ->
                printfn $"MODI {r1}=? {i}"
                failwith "Not implemented"
            | EQLI (r1, CONST c) when known r1 ->
                let v1 = get r1

                match canContain v1 0L, canContain v1 1L with
                | true, _ ->
                    let alu = alu.Add(r1, _0_or_1)
                    let newInst = alu, inst
                    let alu = alu.Remove(r1)
                    newInst :: (eval alu rest)
                | false, true ->
                    let alu = alu.Add(r1, CONST 1L)
                    let newInst = alu, inst
                    let alu = alu.Add(r1, CONST c)
                    newInst :: (eval alu rest)
                | _ -> failwith $"Inconsistent state: {r1} {v1}"
            | EQLI (r1, CONST 0L) when not (known r1) ->
                let newInst = alu, inst
                let alu = alu.Remove(r1)
                newInst :: (eval alu rest)
            | EQLR (r1, r2) when known r1 && known r2 ->
                let v1 = get r1
                let v2 = get r2

                match canContain v1 0L, canContain v2 1L with
                | true, true ->
                    let alu = alu.Add(r1, _0_or_1)
                    let newInst = alu, inst
                    let alu = alu.Remove(r1)
                    newInst :: (eval alu rest)
                | true, false ->
                    let alu = alu.Add(r1, CONST 0L)
                    let newInst = alu, inst
                    let alu = alu.Remove(r1)
                    newInst :: (eval alu rest)
                | false, true ->
                    let alu = alu.Add(r1, CONST 1L)
                    let newInst = alu, inst
                    let alu = alu.Add(r1, v2)
                    newInst :: (eval alu rest)
                | _ -> failwith $"EQLR -> r1 not 0 or 1 {inst} {get r1}"
            | EQLR (r1, r2) when not (known r1) ->
                let newInst = alu, inst
                let alu = alu.Remove(r1)
                newInst :: (eval alu rest)
            | SETI (r1, value) when known r1 ->
                let value =
                    match get r1, value with
                    | v1, INPUT (id, vals) -> INPUT(id, vals |> Map.filter (fun _ i -> canContain v1 i))
                    | _ -> failwith $"Not implemented: {r1} {value}"

                let alu = alu.Add(r1, value)
                let newInst = alu, SETI(r1, value)
                let alu = alu.Remove(r1)
                newInst :: (eval alu rest)
            | SETI (r1, v) -> failwith $"Not implemented: {r1} {v}"
            | _ -> failwith $"not implemented: {inst}"

    let endState: ALU = [ (Z, CONST 0L) ] |> Map
    program |> List.rev |> (eval endState) |> List.rev 

let program5: ALUProgram = propagateALUBack revProgram

let printALUProgram (program: ALUProgram) =
    program |> List.map (fun (alu,inst) -> printInstruction inst; printfn "->" ; printALU alu)

printALUProgram program5

let consolidateValue (value:Value) =
    match value with
    | MULTIPLE s2 when s2.Count = 0 -> INVALID
    | MULTIPLE s2 when s2.Count = 1 -> s2 |> Set.toList |> List.head |> CONST
    | RANGE (min,max) when min = max -> CONST min 
    | RANGE (min,max) when sizeOf value < 30 -> [min..max] |> Set |> MULTIPLE
    | _ -> value 

type OPER =
    | OP_PLUS
    | OP_DIV
    | OP_MULT
    | OP_MOD

let rec testOper (oper:OPER) (value1:Value) (value2:Value) (result:Value) : Value*Value =
    let op = match oper with
             | OP_PLUS -> (*)
             | OP_DIV -> (/)
             | OP_MULT -> (*)
             | OP_MOD -> (%)
    let value1 = consolidateValue value1
    let value2 = consolidateValue value2
    printfn $"testOper {value1 |> v2s} X {value2 |> v2s} -> {result |> v2s} "
    match value1, value2 with
    | CONST _, CONST _ -> (value1,value2)
    | CONST c1, INPUT (id,vals) ->
        let valid2 = vals |> Map.toList
                          |> List.map snd
                          |> List.filter (fun i2 -> op c1 i2 |> (canContain result))
                          |> Set 
        let value2 = INPUT (id,vals |> Map.filter (fun _ v -> valid2.Contains v))
        value1,value2
    | CONST c1, MULTIPLE m2 ->
        let value2 = m2 |> Set.filter (fun v2 -> op c1 v2 |> (canContain result)) |> MULTIPLE
        printfn $"testOper:<- {value1} {value2}"
        value1,value2
    | MULTIPLE m1, CONST c2 -> 
        let pairs = m1 |> Set.toList |> List.map (fun v1 -> v1,c2)
        let valid1 = pairs |> List.map fst |> Set 
        let value1 = m1 |> Set.filter (valid1.Contains) |> MULTIPLE
        printfn $"testOper:<- {value1} {value2}"
        value1,value2 
    | MULTIPLE m1, MULTIPLE m2 ->
        let pairs =
            List.allPairs (m1 |> Set.toList) (m2 |> Set.toList)
            |> List.filter (fun (a,b) -> op a b |> (canContain result))
        let valid1 = pairs |> List.map fst |> Set 
        let valid2 = pairs |> List.map snd |> Set
        let value1 = m1 |> Set.filter (valid1.Contains) |> MULTIPLE
        let value2 = m2 |> Set.filter (valid2.Contains) |> MULTIPLE
        printfn $"testOper:<- {value1} {value2}"
        value1,value2
    | RANGE _,_ when sizeOf value1 < 30 ->
        testOper oper (expand value1) value2 result
    | RANGE (min1,max1),CONST c2 ->
        let min = op min1 c2 
        let max = op max1 c2 
        let minOK = canContain result min
        let maxOK = canContain result max
        let ok = minOK && maxOK
        if ok then value1,value2
        else
            let min1 = if minOK then min1 else min1 + 1L
            let max1 = if maxOK then max1 else max1 - 1L
            testOper oper (RANGE (min1,max1)) value2 result 
        
    | _ -> failwith $"Not implemented: {oper} {value1} {value2} -> {result}"

    

let rec filterOnCalculatedResults (program:ALUProgram) =
    let rec eval (program:ALUProgram) : ALUProgram =
        match program with
        | (outputALU,inst)::(inputALU,prevInst)::rest ->
            printf "Going from :"
            printALU inputALU
            printf "applying instruction: "
            printInstruction inst
            printf "to: "
            printALU outputALU
            match inst with
            | ADDI (r1,v2) -> 
                let result = outputALU.TryFind (r1) |> Option.get
                let v1 = inputALU.TryFind (r1) |> Option.get
                let v1,v2 = testOper OP_PLUS v1 v2 result 
                let inputALU = inputALU.Add(r1,v1)
                (outputALU,ADDI(r1,v2))::(eval ((inputALU,prevInst)::rest))
            | ADDR (r1,r2) ->
                let result = outputALU.TryFind (r1) |> Option.get
                let v1 = inputALU.TryFind (r1) |> Option.get
                let v2 = inputALU.TryFind (r2) |> Option.get
                let v1,v2 = testOper OP_PLUS v1 v2 result 
                let inputALU = inputALU.Add(r1,v1).Add(r2,v2)
                (outputALU,inst)::(eval ((inputALU,prevInst)::rest))
            | MULI (r1,v2) -> 
                let result = outputALU.TryFind (r1) |> Option.get
                let v1 = inputALU.TryFind (r1) |> Option.get
                let v1,v2 = testOper OP_MULT v1 v2 result 
                let inputALU = inputALU.Add(r1,v1)
                (outputALU,MULI(r1,v2))::(eval ((inputALU,prevInst)::rest))
             | MULR (r1,r2) ->
                let result = outputALU.TryFind (r1) |> Option.get
                let v1 = inputALU.TryFind (r1) |> Option.get
                let v2 = inputALU.TryFind (r2) |> Option.get
                let v1,v2 = testOper OP_MULT v1 v2 result 
                let inputALU = inputALU.Add(r1,v1).Add(r2,v2)
                (outputALU,inst)::(eval ((inputALU,prevInst)::rest))
             | DIVI (r1,i2) -> 
                let result = outputALU.TryFind (r1) |> Option.get
                let v1 = inputALU.TryFind (r1) |> Option.get
                let v1,_ = testOper OP_DIV v1 (CONST i2) result 
                let inputALU = inputALU.Add(r1,v1)
                (outputALU,DIVI(r1,i2))::(eval ((inputALU,prevInst)::rest))
             | MODI (r1,i2) -> 
                let result = outputALU.TryFind (r1) |> Option.get
                let v1 = inputALU.TryFind (r1) |> Option.get
                let v1,_ = testOper OP_MOD v1 (CONST i2) result 
                let inputALU = inputALU.Add(r1,v1)
                (outputALU,MODI(r1,i2))::(eval ((inputALU,prevInst)::rest))
              | EQLI (r1,r2) ->
                  match outputALU.TryFind r1 |> Option.get with
                  | CONST 1L -> failwith $"Not implemented {inst}"
                  | CONST 0L -> (outputALU,inst) :: (eval ((inputALU,prevInst)::rest))
                  | MULTIPLE m when m.Contains 0L && m.Contains 1L -> (outputALU,inst) :: (eval ((inputALU,prevInst)::rest))
                  | value -> failwith $"Not implemented {inst} {value}" 
              | EQLR (r1,r2) ->
                  match outputALU.TryFind r1 |> Option.get with
                  | CONST 1L -> failwith $"Not implemented {inst}"
                  | CONST 0L -> (outputALU,inst) :: (eval ((inputALU,prevInst)::rest))
                  | MULTIPLE m when m.Contains 0L && m.Contains 1L -> (outputALU,inst) :: (eval ((inputALU,prevInst)::rest))
                  | value -> failwith $"Not implemented {inst} {value}" 
            | _ -> failwith $"Not implemented: {inst}"
    program |> List.rev |> eval |> List.rev 

filterOnCalculatedResults program5  