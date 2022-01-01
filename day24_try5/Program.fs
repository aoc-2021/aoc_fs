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
        |> sprintf "{ %A }"
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

let printALU (alu:ALU) =
    printf "ALU:"
    printf $"  W: {alu.TryFind W |> Option.get |> v2s}"
    printf $"  X: {alu.TryFind X |> Option.get |> v2s}"
    printf $"  Y: {alu.TryFind Y |> Option.get |> v2s}"
    printfn $"  Z: {alu.TryFind Z |> Option.get |> v2s}"

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

// printProgram program

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
    | RANGE _ -> RANGE(0,v2)
    | INVALID -> INVALID 

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
    | CONST _, RANGE _ -> BOOLS
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
        if v = Set.singleton false then
            INPUT (i, vals |> Map.map (fun _ _ -> 0L))
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
    | INVALID -> false 

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

let removeUnused (program:Program) =
    let rec eval (inUse:Set<Reg>) (program:Program) =
        let used = inUse.Contains
        match program with
        | [] -> []
        | inst::rest ->
            printfn $"inUse: {inUse}"
            printInstruction inst 
            let skip () =
                printfn "Skipping"
                eval inUse rest 
            let cont () =
                printfn "Continuing"
                inst :: (eval inUse rest)
            let continueWith (reg:Reg) =
                printfn $"Continuing with {reg}"
                let inUse = inUse.Add reg
                inst :: (eval inUse rest)
            let continueWithout (reg:Reg) =
                printfn $"Continuing without {reg}"
                let inUse = inUse.Remove(reg)
                inst :: (eval inUse rest)
            match inst with
            | INP _ -> failwith "INP is not supported"
            | ADDI (reg,_) when used reg ->
                cont () 
            | ADDI _ ->
                skip ()
            | ADDR (r1,r2) when used r1 ->
                continueWith r2
            | ADDR _ ->
                skip ()
            | MULI (reg,_) when used reg ->
                cont ()
            | MULR (r1,r2) when used r1 ->
                continueWith r2
            | DIVI (reg,_) when used reg ->
                cont ()
            | DIVI _ ->
                skip ()
            | MODI (reg,_) when used reg ->
                cont ()
            | MODI _ ->
                skip ()
            | EQLI (r1,_) when used r1 ->
                cont ()
            | EQLI _ ->
                skip ()
            | EQLR (r1,r2) when used r1 ->
                continueWith r2
            | EQLR _ ->
                skip ()
            | SETI (r1,_) when used r1 ->
                continueWithout r1 
            | SETI _ ->
                skip ()
            | SETR (r1,r2) when used r1 ->
                let inUse = inUse.Remove r1
                let inUse = inUse.Add r2
                inst :: (eval inUse rest)
            | SETR _ ->
                skip ()
            | _ -> failwith $"Not implemented: {inst}"
    program |> List.rev
            |> eval (Set.singleton Z)
            |> List.rev

let program2 = removeUnused program1

printfn "*****"
printfn "removed unused"
printfn "*****"
printfn ""
printfn $"{program.Length} -> {program1.Length} -> {program2.Length}"
printfn ""

printProgram program2

// OK, now we have a pretty optimal program, only one thing remains... solving the task?
// nah... pushing inputs down to optimize the amount of processing that can be done
// before uncertainty

let rec pushDownInputs (program:Program) =
    match program with
    | [] -> [] 
    | [inst] -> [inst]
    | inst :: inst2 :: rest ->
        match inst with
        | SETI (reg,INPUT _) ->
            let shuffle = 
                match inst2 with
                | ADDI (r2,_) when r2 = reg -> false
                | ADDI _ -> true
                | ADDR (r2,r3) when r2 = reg || r3 = reg -> false
                | ADDR _ -> true
                | MULI (r2,_) when r2 = reg -> false
                | MULI _ -> true
                | MULR (r2,r3) when r2 = reg || r3 = reg -> false
                | MULR _ -> true
                | DIVI (r2,_) when r2 = reg -> false
                | DIVI _ -> true 
                | MODI (r2,_) when r2 = reg -> false
                | MODI _ -> true
                | EQLI (r2,_) when r2 = reg -> false
                | EQLI _ -> true
                | EQLR (r2,r3) when r2 = reg || r3 = reg -> false
                | EQLR _ -> true
                | SETI (_,INPUT _) -> false
                | SETI (r2,_) when r2 = reg ->
                    failwith $"leftover shawowing: {inst2} shadows {inst}"
                | SETI _ -> true
                | SETR (r2,r3) when r2 = reg || r3 = reg ->
                    failwith $"Direct assignment of known value: {r2}<-{r3} following {inst}"
                | SETR _ -> true
                | _ -> failwith $"Not implemented: {inst2}"
            if shuffle then
                inst2 :: (pushDownInputs (inst :: rest))
            else
                inst :: (pushDownInputs (inst2 :: rest))
        | _ -> inst :: (pushDownInputs (inst2 :: rest))      
                    
let program3 = pushDownInputs program2

printfn "******************"
printfn "Pushed down inputs"
printfn "******************"

printProgram program3

// ok, now it's slightly more optimal, time to try to solve the task

let canContain (value:Value) (i:int64) =
    match value with
    | CONST c -> c = i
    | INPUT (_,vals) -> vals |> Map.toList |> List.exists (snd >> (=) i)
    | MULTIPLE s -> s.Contains i
    | RANGE (min,max) -> min <= i && max >= i

let isOdd (i:int64) = i &&& 1L = 1L
let isEven (i:int64) = i &&& 1L = 0L

let isOddValue (value:Value) =
    match value with
    | CONST i -> isOdd i
    | INPUT (_,vals) -> vals.Values |> Seq.map isOdd |> Seq.contains true
    | MULTIPLE s -> s |> Set.map isOdd |> ((=) (Set.singleton false))
    | RANGE (min,max) -> min <> max

let isEvenValue (value:Value) =
    match value with
    | CONST i -> isEven i
    | INPUT (_,vals) -> vals.Values |> Seq.map isEven |> Seq.contains true
    | MULTIPLE s -> s |> Set.map isEven |> ((=) (Set.singleton false))
    | RANGE (min,max) -> min <> max

let filterValue (value:Value) (f:int64->bool) =
    match value with  
    | CONST i -> if f i then CONST i else INVALID
    | INPUT (i,vals) ->
        let vals = vals |> Map.filter (fun _ v -> f v)
        INPUT (i,vals)
    | MULTIPLE s -> s |> Set.filter f |> MULTIPLE 
    | RANGE (min,max) -> RANGE(min,max) // this could be better
    | INVALID -> INVALID 

let rec consolidateLossy (value:Value) =
    match value with
    | CONST c -> CONST c
    | INPUT (i,vals) -> vals.Values |> Set |> MULTIPLE |> consolidateLossy
    | MULTIPLE s when s.Count = 0 -> INVALID
    | MULTIPLE s when s.Count = 1 -> s |> Set.toList |> List.head |> CONST
    | MULTIPLE _ -> value
    | RANGE (min,max) when min = max -> CONST min
    | RANGE (min,max) when min + 20L > max  -> {min..max} |> Set |> MULTIPLE
    | RANGE _ -> value 

type Constraint =
    | C_ZERO of Reg
    | C_EQ of Reg * int64 
    | C_GT of Reg * int64
    | C_GTR of Reg * Reg 
    | C_LT of Reg * int64
    | C_ODD of Reg
    | C_EVEN of Reg
    | C_AND of List<Constraint>
    | C_OR of List<Constraint>
    | C_NONE
    | C_FAIL 

let unknownConstraint (r1:Reg) =
    let c1 = C_OR [C_ZERO r1;C_LT (r1,0L);C_GT(r1,0L)]
    let c2 = C_OR [C_EVEN r1;C_ODD r1]
    C_AND [c1;c2]

let rec cFindR (reg:Reg) (con:Constraint): Constraint =
    match con with
    | C_ZERO r1 when r1 = reg -> con 
    | C_GT (r1,_) when r1 = reg -> con
    | C_GT (r1,_) when r1 = reg -> con
    | C_ODD r1 when r1 = reg -> con
    | C_EVEN r1 when r1 = reg -> con
    | C_EQ (r1,_) when r1 = reg -> con
    | C_AND cs -> cs |> List.map (cFindR reg) |> C_AND 
    | C_OR cs -> cs |> List.map (cFindR reg) |> C_OR
    | _ -> C_NONE 
    
let rec cApplyAddr (con:Constraint) (r1:Reg) (r2:Reg) : Constraint =
    match con with
    | C_ZERO r when r = r1 ->
        let bothZero = C_AND [(C_ZERO r1);(C_ZERO r2)]
        let plusMinus = C_AND [C_GT (r1,0L);C_LT (r2,0L)]
        let minusPlus = C_AND [C_LT (r1,0L);C_GT (r2,0L)]
        let values = C_OR [bothZero;plusMinus;minusPlus]
        let odds = C_AND [C_ODD r1;C_ODD r2]
        let evens = C_AND [C_EVEN r1;C_EVEN r2]
        let oddEvens = C_OR [odds;evens]
        C_AND [values;oddEvens]
    | C_EQ (r,i) when i = 0L -> cApplyAddr (C_ZERO r) r1 r2
    | _ -> failwith $"Not implemented: cApplyAddr {con}"

let rec cApplyAddi (reg:Reg) (value:Value) (con:Constraint) : Constraint =
    let allEven = value |> isOddValue |> not
    let allOdd = value |> isEvenValue |> not 
    match con,value with
    | C_OR ors,_ -> ors |> List.map (cApplyAddi reg value) |> C_OR
    | C_AND ands,_ -> ands |> List.map (cApplyAddi reg value) |> C_AND
    | C_ZERO r,CONST c when r = reg -> C_EQ (r,-c)
    | C_ZERO r,value when r = reg -> failwith $"Not implemented:cApplyAddi {con}"
    | C_ZERO _,_ -> con
    | C_GT (r,i),value when r = reg ->
        match value with
        | CONST v -> C_GT (r,i-v)
        | _ -> failwith $"Not implemented:{con}"
    | C_GT _,_ -> con
    | C_LT (r,i),value when r = reg ->
        match value with
        | CONST v -> C_GT (r,i+v)
        | _ -> failwith $"Not implemented:{con}"
    | C_GT _,_ -> con
 
        
    | C_GT _,_ -> con 
    | C_LT (r,i),value when r = reg -> failwith $"Not implemented:cApplyAddi {con}"
    | C_LT _,_ -> con
    | C_ODD r,_ when r = reg && allEven -> con
    | C_ODD r,_ when r = reg && allOdd -> C_EVEN r
    | C_ODD _,_ -> con 
    | C_EVEN r,_ when r = reg && allEven -> con
    | C_EVEN r,_ when r = reg && allOdd -> C_ODD r
    | C_EVEN _,_ -> con
        
    | _ -> failwith $"Not implemented:cApplyAddi {con}" 
        
let rec cApplyMulr (r1:Reg) (r2:Reg) (con:Constraint) : Constraint =
    match con with
    | C_OR ors -> ors |> List.map (cApplyMulr r1 r2) |> C_OR
    | C_AND ands -> ands |> List.map (cApplyMulr r1 r2) |> C_AND
    | C_ZERO r when r1 = r -> C_OR [C_ZERO r1;C_ZERO r2]
    | C_ZERO _ -> con
    | C_EQ (r,i) when r1 = r && i = 0L -> cApplyMulr r1 r2 (C_ZERO r)
    | C_EQ (r,i) when r1 = r && i < 0L ->
        let aboveBelow = C_AND [C_GT (r1,0L);(C_LT (r2,0L))]
        let belowAbove = C_AND [C_LT (r1,0L);(C_GT (r2,0L))]
        let range = C_OR [aboveBelow;belowAbove]
        let oddEven =
            if isOdd i then C_AND [C_ODD r1;C_ODD r2]
            else C_OR [C_EVEN r1;C_EVEN r2]
        C_AND [range;oddEven]
    | C_EQ (r,i) when r1 = r && i > 0L ->
        let above = C_AND [C_GT (r1,0L);(C_GT (r2,0L))]
        let below = C_AND [C_LT (r1,0L);(C_LT (r2,0L))]
        let range = C_OR [above;below]
        let oddEven =
            if isOdd i then C_AND [C_ODD r1;C_ODD r2]
            else C_OR [C_EVEN r1;C_EVEN r2]
        C_AND [range;oddEven]
    | C_EQ _ -> con 
    | C_GT (r,i) when r = r1 && i >= 0L ->
        let bothAbove = C_AND [C_GT (r1,0L);(C_GT (r2,0L))]
        let bothBelow = C_AND [C_LT (r1,0L);(C_LT (r2,0L))]
        C_OR [bothAbove;bothBelow]
    | C_GT _ -> con
    | C_LT (r,i) when r = r1 && i <= 0L ->
        let aboveBelow = C_AND [C_GT (r1,0L);(C_LT (r2,0L))]
        let belowAbove = C_AND [C_LT (r1,0L);(C_GT (r2,0L))]
        C_OR [aboveBelow;belowAbove]
    | C_LT _ -> con
    | C_ODD r when r = r1 -> C_AND [C_ODD r1;C_ODD r2]
    | C_ODD _ -> con
    | C_EVEN r when r = r1 -> C_OR [C_EVEN r1;C_EVEN r2]
    | C_EVEN _ -> con 
    | _ -> failwith $"Not implemented: {con}"

let rec cApplySeti (reg:Reg) (value:Value) (con:Constraint) =
    match con with
    | C_AND ands -> ands |> List.map (cApplySeti reg value) |> C_AND  
    | C_OR ors -> ors |> List.map (cApplySeti reg value) |> C_OR
    | C_ZERO r when r = reg ->
        if canContain value 0L then
            C_NONE
        else
            C_FAIL 
    | C_ZERO _ -> con
    | C_GT (r, i) when r = reg ->
        if largest value > i then
            C_NONE
        else
            C_FAIL
    | C_GT _ -> con 
    | C_LT (r, i) when r = reg ->
        if smallest value < i then
            C_NONE
        else
            C_FAIL
    | C_LT _ -> con
    | C_ODD r when r = reg ->
        if isOddValue value then C_NONE
        else C_FAIL 
    | C_ODD _ -> con 
    | C_EVEN r when r = reg ->
        if isEvenValue value then C_NONE
        else C_FAIL 
    | C_EVEN _ -> con 
       
    | _ -> failwith $"Not implemented: {con}"

let rec cApplyEqli (reg:Reg) (value:Value) (con:Constraint) =
    match con with
    | C_OR ors -> ors |> List.map (cApplyEqli reg value) |> C_OR
    | C_AND ands -> ands |> List.map (cApplyEqli reg value) |> C_AND

    | C_ZERO r when r = reg && value = (CONST 0L) ->
        C_OR [C_GT (r,0L);C_LT (r,0L)]
    | C_ZERO r when r = reg -> C_NONE 
    | C_ZERO _ -> con
    | C_EQ (r,1L) when r= reg ->
        match value with
        | CONST c -> C_EQ (r,c)
        | MULTIPLE s when s.Count < 3 ->
            s |> Set.toList
              |> List.map (fun s -> C_EQ(r,s)) |> C_OR
        | _ -> C_AND [C_LT (r,largest value + 1L);C_GT (r,smallest value - 1L)]
    | C_EQ (r,0L) -> cApplyEqli reg value (C_ZERO r) 
    | C_GT (r,1L) when r = reg -> C_FAIL
    | C_GT (r,0L) when r = reg -> cApplyEqli reg value (C_EQ (r,1L))
    | C_GT (r,i) when r = reg && i < 0L -> C_NONE
    | C_GT _ -> con  
    | C_LT (r,0L) when r = reg -> C_FAIL
    | C_LT (r,1L) when r = reg -> cApplyEqli reg value (C_ZERO r)
    | C_LT (r,i) when r = reg && i > 1L -> C_NONE
    | C_LT _ -> con
    | C_ODD r -> cApplyEqli reg value (C_EQ (r,0L))
    | C_EVEN r -> cApplyEqli reg value (C_ZERO r)
    | _ -> failwith $"Not implemented {con}"

let checkConstraints (program:Program) : Program =
    let rec check (con:Constraint) (program:Program) =
        match program with
        [] -> []
        | inst :: rest ->
            printfn $"Checking: {inst}"
            printfn $"Constraint: {con}"
            printfn "--"
            match inst with
            | ADDR (r1,r2) ->
                let con = cApplyAddr con r1 r2
                check con rest
            | ADDI (r1,i) ->
                let con = cApplyAddi r1 i con
                check con rest
            | MULR (r1,r2) ->
                let con = cApplyMulr r1 r2 con
                check con rest
            | SETI (r1,value) ->
                let newCon = cApplySeti r1 value con
                check con rest
            | EQLI (r,value) ->
                let con = cApplyEqli r value con
                check con rest 
            | _ -> failwith $"Not implemented {inst}"
            
    program |> List.rev
            |> check (C_ZERO Z) 
            |> List.rev
            
let program4 = checkConstraints program3






    