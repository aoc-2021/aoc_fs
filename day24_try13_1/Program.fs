open System.IO

let file = File.ReadAllLines "input.txt"

type Reg =
    | W
    | X
    | Y
    | Z

type Sources(inputs: list<Map<int, Set<int64>>>) =
    member this.Inputs = inputs

    member this.IntersectWith(other: Sources) : Option<Sources> =
        if this.Inputs.IsEmpty then
            Some(other)
        elif other.Inputs.IsEmpty then
            Some(this)
        else
            let intersect (map1: Map<int, Set<int64>>, map2: Map<int, Set<int64>>) : Option<Map<int, Set<int64>>> =
                let keys1 = map1.Keys |> Set
                let keys2 = map2.Keys |> Set
                let shared = Set.intersect keys1 keys2

                let keys1: Set<int> =
                    keys1 |> Set.filter (shared.Contains >> not)

                let keys2: Set<int> =
                    keys2 |> Set.filter (shared.Contains >> not)

                let shared =
                    shared
                    |> Set.map (fun i -> i, map1.TryFind i |> Option.get, map2.TryFind i |> Option.get)

                let shared =
                    shared
                    |> Set.map (fun (i, set1, set2) -> i, Set.intersect set1 set2)

                if shared
                   |> Set.filter (fun (i, deps) -> deps = Set.empty)
                   |> Set.isEmpty
                   |> not then
                    None
                else
                    let shared = shared |> Set.toList

                    let deps1 =
                        keys1
                        |> Set.map (fun i -> i, map1.TryFind i |> Option.get)
                        |> Set.toList

                    let deps2 =
                        keys2
                        |> Set.map (fun i -> i, map2.TryFind i |> Option.get)
                        |> Set.toList

                    let allDeps =
                        List.concat [ shared; deps1; deps2 ] |> Map

                    Some(allDeps)

            let pairs = List.allPairs inputs other.Inputs

            let deps =
                pairs
                |> List.map intersect
                |> List.filter Option.isSome
                |> List.map Option.get

            if deps.IsEmpty then
                None
            else
                Some(Sources(deps))

    member this.Normalize () =
        let allInputs = [1L..9L] |> Set
        if inputs.IsEmpty || inputs.Length = 1 then this
        else
            let mergeGroup (keys:Set<int>,maps:list<Map<int,Set<int64>>>) =
                let onlySingleDiff =
                    let allValues (id:int) = maps |> List.map (fun m -> m.TryFind id |> Option.get)
                    let allSame (sets: list<Set<int64>>) = sets |> Set |> Set.count = 1
                    let diffValues = allValues >> allSame >> not 
                    printfn $"XYZ:: {keys}:{keys.GetType} {maps}:{maps.GetType}"
                    let diffValues = keys |> Set.toList |> List.filter diffValues
                    printfn $"diffValues={diffValues}"
                    diffValues.Length <= 1
                let mergeAll () : Map<int,Set<int64>> = 
                    let allValues (id:int) = maps |> List.map (fun m -> m.TryFind id |> Option.get)
                    let mergedValues = allValues >> Set.unionMany
                    keys |> Set.toList |> List.map (fun i -> (i,mergedValues i) )|> Map
                if onlySingleDiff then
                    [ mergeAll () ]
                else
                    maps 
            let groups : list<Set<int>*list<Map<int,Set<int64>>>> =
                inputs |> List.groupBy (fun (m:Map<int,Set<int64>>) -> m.Keys |> Set)
            let merged = groups |> List.map mergeGroup |> List.concat
            let merged = merged |> List.map (Map.filter (fun _ v -> v <> allInputs))
            let merged = merged |> List.filter (Map.isEmpty >> not)
            merged |> Sources
    member this.unionWith(other: Sources) =
        Sources(List.concat [ inputs; other.Inputs ])

    static member unionMany(sl: list<Sources>) =
        let rec merge (sl: list<Sources>) =
            match sl with
            | [] -> failwith $"Not supported: unionMany {sl}"
            | [ last ] -> last
            | a :: b :: rest -> (a.unionWith b) :: rest |> merge

        merge sl

    override this.ToString() =
        let inputsToString (is: Set<int64>) =
            is
            |> Set.toList
            |> List.map string
            |> String.concat ""

        let pairToString (k, vs) = $"{k}:{vs |> inputsToString}"

        inputs
        |> List.map
            (fun m ->
                m
                |> Map.toList
                |> List.map pairToString
                |> String.concat ",")
        |> String.concat ";"

let src1 =
    Sources(
        [ [ (1, [ 2L; 6L; 4L ] |> Set) ] |> Map
          [ (1, [ 2L; 3L; 4L ] |> Set) ] |> Map ]
    )

let src2 =
    Sources(
        [ [ (1, [ 1L; 2L; 3L ] |> Set)
            (2, Set.singleton 2L) ]
          |> Map ]
    )

let srcx:Sources = src1.IntersectWith src2 |> Option.get 
printfn $"XXX1 {srcx}"
let srxy = srcx.Normalize ()
printfn $"XXXN {srxy}"

type SourcedNumber(value: int64, sources: Sources) =
    interface System.IComparable with
        override this.CompareTo(other) =
            match other with
            | :? SourcedNumber as other ->
                if value > other.Value then 1
                else if value < other.Value then -1
                else 0
            | _ -> 1

    member this.IntersectWith(other: SourcedNumber) : Option<SourcedNumber> =
        if value <> other.Value then
            None
        else
            let sources = sources.IntersectWith other.Sources

            sources
            |> Option.map (fun sources -> SourcedNumber(value, sources))

    member this.BinaryOperation (op: int64 -> int64 -> int64) (other: SourcedNumber) : Option<SourcedNumber> =
        let value = op this.Value other.Value
        let sources = this.Sources.IntersectWith other.Sources

        sources
        |> Option.map (fun i -> SourcedNumber(value, i))

    member this.Normalize () = SourcedNumber(value,sources.Normalize())
    override this.ToString() =
        if sources.Inputs.IsEmpty then
            value |> string
        else
            $"'{value}[{sources}]"

    member this.Value = value
    member this.Sources = sources
    static member ofAnonymous(i: int64) = SourcedNumber(i, Sources [])

type SourcedValue(vals: Map<int64, SourcedNumber>) =
    member this.Vals = vals

    static member Input(id: int) =
        let input (i: int) =
            SourcedNumber(
                i |> int64,
                Sources(
                    [ (id, [ i |> int64 ] |> Set) ]
                    |> Map
                    |> List.singleton
                )
            )

        [ 1 .. 9 ]
        |> List.map (fun i -> i |> int64, input i)
        |> Map
        |> SourcedValue

    member this.ContainsAny(pred: int64 -> bool) =
        vals.Keys |> Seq.tryFind pred |> Option.isSome

    member this.ContainsNone(pred: int64 -> bool) =
        vals.Keys |> Seq.tryFind pred |> Option.isNone

    member this.isNatural() = vals.Keys |> Seq.min >= 0L

    member this.intersectWithConst(num: SourcedNumber) : Option<SourcedNumber> =
        let addSource (sn: SourcedNumber) =
            let sources = sn.Sources.IntersectWith num.Sources

            sources
            |> Option.map (fun sources -> SourcedNumber(sn.Value, sources))

        let value = vals.TryFind num.Value

        if value.IsNone then
            None
        else
            let value = value |> Option.get
            value.IntersectWith num

    member this.intersectWithValues(other: SourcedValue) : Option<SourcedValue> =
        let sharedInts =
            Set.intersect (vals.Keys |> Set) (other.Vals.Keys |> Set)

        sharedInts
        |> Set.map (fun i -> i, vals.TryFind i |> Option.get, other.Vals.TryFind i |> Option.get)
        |> Set.map (fun (i, num1, num2) -> i, num1.IntersectWith num2)
        |> Set.filter (fun (_, num) -> num.IsSome)
        |> Set.map (fun (i, num) -> i, num |> Option.get)
        |> Set.toList
        |> Map
        |> (fun m ->
            if m.IsEmpty then
                None
            else
                Some(m |> SourcedValue))

    member this.BinaryOperationWithConst (op: int64 -> int64 -> int64) (other: SourcedNumber) =
        let mergeEqualNums (ns: list<SourcedNumber>) =
            let value = ns.Head.Value

            let sources =
                ns
                |> List.map (fun v -> v.Sources)
                |> Sources.unionMany

            SourcedNumber(value, sources)

        let result =
            vals
            |> Map.toList
            |> List.map snd
            |> List.map (fun v -> v.BinaryOperation op other)
            |> List.filter (fun v -> v.IsSome)
            |> List.map (fun v -> v |> Option.get)
            |> List.groupBy (fun v -> v.Value)
            |> List.map (fun (v, vs) -> (v, mergeEqualNums vs))
            |> Map
            |> SourcedValue

        result

    member this.BinaryOperationWithValue (op: int64 -> int64 -> int64) (other: SourcedValue) =
        let mergeEqualNums (ns: list<SourcedNumber>) =
            let value = ns.Head.Value

            let sources =
                ns
                |> List.map (fun v -> v.Sources)
                |> Sources.unionMany

            SourcedNumber(value, sources)

        let result =
            vals
            |> Map.toList
            |> List.map snd
            |> List.map (fun v -> other.BinaryOperationWithConst op v)
            |> List.map (fun v -> v.Vals.Values |> Seq.toList)
            |> List.concat
            |> List.groupBy (fun v -> v.Value)
            |> List.map (fun (v, vs) -> (v, mergeEqualNums vs))
            |> Map
            |> SourcedValue

        result

    member this.Normalize () = vals |> Map.map (fun _ v -> v.Normalize ()) |> SourcedValue
    
    static member ofInts(inputs: List<int>) =
        inputs
        |> List.map (fun i -> i |> int64, SourcedNumber.ofAnonymous i)
        |> Map
        |> SourcedValue

    override this.ToString() =
        vals
        |> Map.values
        |> Seq.map (sprintf "%A")
        |> String.concat " "

let n1 =
    SourcedNumber(1, Sources [ ([ (1, [ 2L; 3L ] |> Set) ] |> Map) ])

let n2 =
    SourcedNumber(4, Sources [ ([ (3, [ 1L; 4L ] |> Set) ] |> Map) ])

let nx =
    SourcedNumber(
        2,
        Sources [ [ (2, [ 2L; 4L ] |> Set)
                    (2, [ 5L; 6L ] |> Set) ]
                  |> Map ]
    )

let v1 =
    [ (1L, n1); (3L, n2) ] |> Map |> SourcedValue

let vx = v1.BinaryOperationWithConst(*) nx

let vx2 =
    vx.BinaryOperationWithConst(fun _ _ -> 1L) n1

printfn $"vx={vx}"
printfn $"vx2={vx2}"

type Value =
    | UNKNOWN
    | CONST of SourcedNumber
    | FROM of int64
    | TO of int64
    | VALUES of SourcedValue
    | REG of Reg
    | VOID

let _0 = SourcedNumber.ofAnonymous 0L |> CONST
let _1 = SourcedNumber.ofAnonymous 1L |> CONST
let binary = SourcedValue.ofInts [ 0; 1 ]

let value2String (value: Value) =
    match value with
    | UNKNOWN -> "?"
    | CONST i -> i |> string
    | FROM i -> $"{i}..."
    | TO i -> $"...{i}"
    | VALUES v -> v |> string
    | REG r -> r |> string
    | VOID -> "∅"

let normalizeValue (value:Value) =
    let skip = value 
    match value with
    | UNKNOWN -> skip 
    | CONST i -> i.Normalize () |> CONST 
    | FROM i -> skip 
    | TO i -> skip 
    | VALUES v -> v.Normalize() |> VALUES 
    | REG r -> skip 
    | VOID -> skip 

type Param =
    | R of Reg
    | I of int64

let param2String (param: Param) =
    match param with
    | R r -> $"{r}"
    | I i -> $"{i}"

type Inst =
    | INP of Reg * Value
    | ADD of Reg * Value
    | MUL of Reg * Value
    | DIV of Reg * int64
    | MOD of Reg * int64
    | EQL of Reg * Value
    | SET of Reg * Value
    | NOP

let inst2String (inst: Inst) =
    match inst with
    | INP (r, value) -> $"{r} ⭊ {value |> value2String}"
    | ADD (r, value) -> $"{r} := {r} + {value |> value2String}"
    | MUL (r, value) -> $"{r} := {r} * {value |> value2String}"
    | DIV (r, value) -> $"{r} := {r} / {value}"
    | MOD (r, value) -> $"{r} := {r} %% {value}"
    | EQL (r, value) -> $"{r} := {r} = {value |> value2String}"
    | SET (r, value) -> $"{r} := {value |> value2String}"
    | NOP -> "...        "

let intersect (a: Value) (b: Value) =
    match a, b with
    | UNKNOWN, _ -> b
    | _, UNKNOWN -> a
    | CONST a, CONST b -> a.IntersectWith b |> Option.get |> CONST
    | VALUES values, CONST c ->
        values.intersectWithConst c
        |> Option.get // assuming that there is an intersection
        |> CONST
    | VALUES v1, VALUES v2 -> v1.intersectWithValues v2 |> Option.get |> VALUES
    | _ -> a
    | _ -> failwith $"Not implemented: intersect {a} {b}"

type ALU(regs: Map<Reg, Value>) =
    member this.Get(reg: Reg) = regs.TryFind reg |> Option.get
    member this.Set (reg: Reg) (value: Value) = regs.Add(reg, value) |> ALU

    member this.SyncRegs (other: ALU) (regs: list<Reg>) =
        let rec sync (alu1: ALU) (alu2: ALU) (regs: list<Reg>) =
            match regs with
            | [] -> alu1, alu2
            | reg :: rest ->
                if alu1.Get reg = VOID || alu2.Get reg = VOID then
                    sync alu1 alu2 rest
                else
                    let value = intersect (alu1.Get reg) (alu2.Get reg)
                    let alu1 = alu1.Set reg value
                    let alu2 = alu2.Set reg value
                    sync alu1 alu2 rest

        sync this other regs

    member this.SyncVoid (other: ALU) (regs: List<Reg>) =
        let rec sync (alu1: ALU) (alu2: ALU) (regs: list<Reg>) =
            match regs with
            | [] -> alu1, alu2
            | reg :: rest ->
                if alu1.Get reg = VOID || alu2.Get reg = VOID then
                    let alu1 = alu1.Set reg VOID
                    let alu2 = alu2.Set reg VOID
                    sync alu1 alu2 rest
                else
                    sync alu1 alu2 rest

        sync this other regs

    member this.Normalize () : ALU =
        regs |> Map.map (fun _ v -> normalizeValue v) |> ALU 
    
    override this.ToString() =
        ALU.allRegs
        |> List.filter (fun r -> this.Get r <> UNKNOWN)
        |> List.map (fun r -> r, this.Get r)
        |> List.map (fun (r, value) -> $"{r}:{value |> value2String}")
        |> String.concat ","

    static member allRegs = [ W; X; Y; Z ]
    static member allRegsExcept(reg: Reg) = ALU.allRegs |> List.filter ((<>) reg)

    static member initial =
        ALU.allRegs
        |> List.map (fun i -> i, CONST(SourcedNumber.ofAnonymous 0L))
        |> Map
        |> ALU

    static member unknown =
        ALU.allRegs
        |> List.map (fun i -> i, UNKNOWN)
        |> Map
        |> ALU

    static member final =
        ALU.allRegs
        |> List.map (fun i -> i, VOID)
        |> Map
        |> ALU
        |> (fun alu -> alu.Set Z _0)

let readInstructions (file: string) =
    let input =
        File.ReadAllLines file
        |> Array.toList
        |> List.map (fun s -> s.Split " ")

    let parseLine (line: string []) (inputNo: int) : int * Inst =
        let toReg (s: string) =
            match s with
            | "w" -> W
            | "x" -> X
            | "y" -> Y
            | "z" -> Z

        let toValue (s: string) =
            match s with
            | "w" -> REG W
            | "x" -> REG X
            | "y" -> REG Y
            | "z" -> REG Z
            | _ -> int64 s |> SourcedNumber.ofAnonymous |> CONST

        match line with
        | [| "add"; _; "0" |] -> inputNo, NOP
        | [| "add"; reg; value |] -> inputNo, ADD(toReg reg, toValue value)
        | [| "mul"; reg; "0" |] -> inputNo, SET((toReg reg), _0)
        | [| "mul"; _; "1" |] -> inputNo, NOP
        | [| "mul"; reg; value |] -> inputNo, MUL(toReg reg, toValue value)
        | [| "div"; _; "1" |] -> inputNo, NOP
        | [| "div"; reg; value |] -> inputNo, DIV(toReg reg, int64 value)
        | [| "mod"; reg; value |] -> inputNo, MOD(toReg reg, int64 value)
        | [| "eql"; reg; value |] -> inputNo, EQL(toReg reg, toValue value)
        | [| "inp"; reg |] -> inputNo + 1, INP((toReg reg), VALUES(SourcedValue.Input inputNo))

    let rec parse (lines: list<string []>) (inputNo: int) =
        match lines with
        | [] -> []
        | line :: rest ->
            let inputNo, inst = parseLine line inputNo
            inst :: (parse rest inputNo)

    parse input 0

let narrowAdd (reg: Value) (param: Value) (output: Value) : Value * Value * Value =
    let skip = reg, param, output

    match reg, param, output with
    | UNKNOWN, UNKNOWN, _ -> skip
    | UNKNOWN, _, UNKNOWN -> skip
    | FROM _, UNKNOWN, FROM _ -> skip
    | FROM from, _, CONST c when c.Value = 0L ->
        let param = intersect param (TO(-from))
        // TODO : filter reg 
        reg, param, output
    | FROM _,_,FROM _ -> skip
    | CONST c, _, _ when c.Value = 0L ->
        let value = intersect param output
        reg, value, value
    | _, _, VOID -> VOID, param, VOID
    | VALUES v1, CONST c, _ ->
        let result =
            v1.BinaryOperationWithConst(+) c |> VALUES

        let output = intersect result output
        // TODO: filter input
        reg, param, result
    | _ -> failwith $"narrowAdd: Not implemented: {reg} {param} {output}"

let narrowMul (reg: Value) (param: Value) (output: Value) : Value * Value * Value =
    let skip = reg, param, output

    match reg, param, output with
    | UNKNOWN, UNKNOWN, _ -> skip
    | _, UNKNOWN, UNKNOWN -> skip
    | UNKNOWN, _, UNKNOWN -> skip
    | CONST c, VALUES v, _ ->
        let result =
            v.BinaryOperationWithConst(*) c |> VALUES

        let output = intersect output result
        // TODO : filter input
        reg, param, output
    | FROM 0L, VALUES v, UNKNOWN when v.isNatural () -> reg, param, FROM 0L
    | FROM 0L, VALUES v, FROM 0L when v.ContainsAny((<>) 0L) -> skip
    | VALUES v1, VALUES v2, _ ->
        let result =
            v1.BinaryOperationWithValue(*) v2 |> VALUES

        let output = intersect output result
        // TODO: Filter
        reg, param, output
    | _ -> failwith $"narrowMul: Not implemented: {reg} {param} {output}"

let narrowDiv (reg: Value) (param: int64) (output: Value) : Value * Value =
    let skip = reg, output

    match reg, output with
    | UNKNOWN, UNKNOWN -> skip
    | FROM 0L, UNKNOWN -> FROM 0L, FROM 0L
    | FROM 0L, FROM 0L -> skip
    | _ -> failwith $"narrowDiv: Not implemented: {reg} {param} {output}"

let narrowMod (reg: Value) (param: int64) (output: Value) : Value * Value =
    let skip = reg, output

    let outputRange =
        SourcedValue.ofInts [ 0 .. (param |> int) - 1 ]
        |> VALUES

    match reg, output with
    | UNKNOWN, UNKNOWN -> FROM 0L, outputRange
    | FROM 0L, CONST c when c.Value = 0L -> skip
    | FROM 0L, VALUES v when v.Vals.ContainsKey 0L -> skip
    | CONST a, _ ->
        let result =
            a.BinaryOperation(fun a b -> a % b) (SourcedNumber.ofAnonymous param)

        let result = result |> Option.get |> CONST
        let value = intersect output result
        reg, value
    | _ -> failwith $"narrowMod: Not implemented: {reg} {param} {output}"

let narrowEql (reg: Value) (param: Value) (output: Value) : Value * Value * Value =
    let skip = reg, param, output

    match reg, param, output with
    | UNKNOWN, _, UNKNOWN -> UNKNOWN, param, VALUES binary
    | _ -> skip // TOOD
    | _ -> failwith $"narrowEql: Not implemented: {reg} {param} {output}"

type Step(inst: Inst, input: ALU, output: ALU) =
    member this.Inst = inst
    member this.Input = input
    member this.Output = output
    member this.SetInput(input: ALU) = Step(inst, input, output)
    member this.SetOutput(output: ALU) = Step(inst, input, output)

    member this.NarrowValues() =
        match inst with
        | NOP -> this
        | INP (r, value) ->
            let value = intersect (output.Get r) value
            let inst = INP(r, value)
            let output = output.Set r value
            let input = input.Set r VOID
            Step(inst, input, output)
        | SET (r, value) ->
            let value = intersect (output.Get r) value
            let output = output.Set r value
            let input = input.Set r VOID
            Step(inst, input, output)
        | ADD (r, REG other) ->
            let value = input.Get r
            let param = input.Get other
            let result = output.Get r
            let value, param, result = narrowAdd value param result
            let input = input.Set r value
            let input = input.Set other param
            let output = output.Set r result
            Step(inst, input, output)
        | ADD (r, param) ->
            let value = input.Get r
            let result = output.Get r
            let value, _, result = narrowAdd value param result
            let input = input.Set r value
            let output = output.Set r result
            Step(inst, input, output)
        | MUL (r, REG other) ->
            let value = input.Get r
            let param = input.Get other
            let result = output.Get r
            let value, param, result = narrowMul value param result
            let input = input.Set r value
            let input = input.Set other param
            let output = output.Set r result
            Step(inst, input, output)
        | DIV (r, i) ->
            let value = input.Get r
            let result = output.Get r
            let value, result = narrowDiv value i result
            let input = input.Set r value
            let output = output.Set r result
            Step(inst, input, output)
        | MOD (r, i) ->
            let value = input.Get r
            let result = output.Get r
            let value, result = narrowMod value i result
            let input = input.Set r value
            let output = output.Set r result
            Step(inst, input, output)
        | EQL (r, REG other) ->
            let value = input.Get r
            let param = input.Get other
            let result = output.Get r
            let value, param, result = narrowEql value param result
            let input = input.Set r value
            let input = input.Set other param
            let output = output.Set r result
            Step(inst, input, output)
        | EQL (r, param) ->
            let value = input.Get r
            let result = output.Get r
            let value, _, result = narrowEql value param result
            let input = input.Set r value
            let output = output.Set r result
            Step(inst, input, output)
        | _ ->
            printfn $"Not implemented {inst}"
            this

    member this.SyncInternal() : Step =
        match inst with
        | INP (r, _) ->
            let input, output =
                input.SyncRegs output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | ADD (r, _) ->
            let input, output =
                input.SyncRegs output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | MUL (r, _) ->
            let input, output =
                input.SyncRegs output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | DIV (r, _) ->
            let input, output =
                input.SyncRegs output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | MOD (r, _) ->
            let input, output =
                input.SyncRegs output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | EQL (r, _) ->
            let input, output =
                input.SyncRegs output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | SET (r, _) ->
            let input, output =
                input.SyncRegs output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | NOP ->
            let input, output = input.SyncRegs output ALU.allRegs
            Step(inst, input, output)

    member this.SyncVoidInternal() =
        match inst with
        | INP (r, _) ->
            let input, output =
                input.SyncVoid output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | ADD (r, REG other) ->
            let regs =
                ALU.allRegs
                |> List.filter (fun x -> x <> r && x <> other)

            let input, output = input.SyncVoid output regs
            Step(inst, input, output)
        | ADD (r, CONST _) ->
            let input, output =
                input.SyncVoid output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | MUL (r, REG other) ->
            let regs =
                ALU.allRegs
                |> List.filter (fun x -> x <> r && x <> other)

            let input, output = input.SyncVoid output regs
            Step(inst, input, output)
        | MUL (r, CONST _) ->
            let input, output =
                input.SyncVoid output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | DIV (r, _) ->
            let input, output =
                input.SyncVoid output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | MOD (r, _) ->
            let input, output =
                input.SyncVoid output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | EQL (r, REG other) ->
            let regs =
                ALU.allRegs
                |> List.filter (fun x -> x <> r && x <> other)

            let input, output = input.SyncVoid output regs
            Step(inst, input, output)
        | EQL (r, CONST _) ->
            let input, output =
                input.SyncVoid output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | SET (r, REG other) ->
            let regs =
                ALU.allRegs
                |> List.filter (fun x -> x <> r && x <> other)

            let input, output = input.SyncVoid output regs
            Step(inst, input, output)
        | SET (r, CONST _) ->
            let input, output =
                input.SyncVoid output (ALU.allRegsExcept r)

            Step(inst, input, output)
        | NOP ->
            let input, output = input.SyncVoid output ALU.allRegs
            Step(inst, input, output)
        | _ -> failwith $"Not implemented: {inst}"

    member this.Update() =
        let step = this.NarrowValues()
        let step = step.SyncInternal()
        step

    member this.Normalize () =
        let input = input.Normalize ()
        let output = output.Normalize ()
        Step (inst,input,output)
    
    override this.ToString() =
        $"⍗ {inst |> inst2String, -15} IN:{input, -30}   OUT:{output, -30}"

let instructionsToProgram (instructions: list<Inst>) =
    let rec convert (instructions: list<Inst>) =
        match instructions with
        | [] -> []
        | [ inst ] -> [ Step(inst, ALU.unknown, ALU.final) ]
        | inst :: rest ->
            Step(inst, ALU.unknown, ALU.unknown)
            :: convert rest

    let program = convert instructions
    let first = program.Head

    let first =
        Step(first.Inst, ALU.initial, first.Output)

    first :: program.Tail

let readInput (filename: string) : List<Step> =
    readInstructions filename |> instructionsToProgram

let program = readInput "input.txt"

let rec syncDown (program: List<Step>) =
    match program with
    | [] -> []
    | [ final ] -> [ final ]
    | step1 :: step2 :: rest ->
        let output1, input2 =
            step1.Output.SyncRegs step2.Input ALU.allRegs

        let output1, input2 = output1.SyncVoid input2 ALU.allRegs
        let step1 = step1.SetOutput output1
        let step2 = step2.SetInput input2
        step1 :: (syncDown (step2 :: rest))

let syncUp (program: List<Step>) =

    let rec sync (program: List<Step>) =
        match program with
        | [] -> []
        | [ first ] -> [ first ]
        | step2 :: step1 :: rest ->
            let output1, input2 =
                step1.Output.SyncRegs step2.Input ALU.allRegs

            let output1, input2 = output1.SyncVoid input2 ALU.allRegs
            let step1 = step1.SetOutput output1
            let step2 = step2.SetInput input2
            step2 :: (sync (step1 :: rest))

    program |> List.rev |> sync |> List.rev

let solveStep (program: List<Step>) =
    program
    |> List.map (fun s -> s.NarrowValues())
    |> List.map (fun s -> s.SyncInternal())
    |> List.map (fun s -> s.SyncVoidInternal())
    |> List.map (fun s -> s.Normalize())
    |> syncDown
    |> syncUp

let rec solveSteps (n: int) (program: list<Step>) =
    if n < 1 then
        program
    else
        let program = solveStep program
        solveSteps (n - 1) program

solveSteps 19 program
|> List.map (printfn "%A")
|> ignore
