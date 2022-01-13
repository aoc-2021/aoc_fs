open System.IO

let file = File.ReadAllLines "input.txt"

type Reg =
    | W
    | X
    | Y
    | Z

type Sources(inputs: List<Map<int, Set<int64>>>) =
    member this.Inputs = inputs

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

type SourcedNumber(value: int64, sources: Sources) =
    interface System.IComparable with
        override this.CompareTo(other) =
            match other with
            | :? SourcedNumber as other ->
                if value > other.Value then 1
                else if value < other.Value then -1
                else 0
            | _ -> 1

    override this.ToString() =
        if sources.Inputs.IsEmpty then
            value |> string
        else
            $"'{value}[{sources}]"

    member this.Value = value
    member this.Sources = sources
    static member ofAnonymous(i: int64) = SourcedNumber(i, Sources [])

type SourcedValue(vals: Set<SourcedNumber>) =
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
        |> List.map input
        |> Set
        |> SourcedValue

    static member ofInts(inputs: List<int>) =
        inputs
        |> Set
        |> Set.map SourcedNumber.ofAnonymous
        |> SourcedValue

    override this.ToString() =
        vals
        |> Set.map (sprintf "%A")
        |> String.concat " "

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
let binary = SourcedValue.ofInts ([ 0; 1 ])

let value2String (value: Value) =
    match value with
    | UNKNOWN -> "?"
    | CONST i -> i |> string
    | FROM i -> $"{i}..."
    | TO i -> $"...{i}"
    | VALUES v -> v |> string
    | REG r -> r |> string
    | VOID -> "∅"

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
    | SET (r, value) -> $"{r} := {value}"
    | NOP -> "...        "
    | _ -> inst |> string

let intersect (a: Value) (b: Value) =
    match a, b with
    | UNKNOWN, _ -> b
    | _, UNKNOWN -> a
    | _ -> failwith $"Not implementend: intersect {a} {b}"

type ALU(regs: Map<Reg, Value>) =
    member this.Get(reg: Reg) = regs.TryFind reg |> Option.get
    member this.Set (reg: Reg) (value: Value) = regs.Add(reg, value) |> ALU

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
        |> (fun alu -> alu.Set Z (_0))

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
    let skip = reg,param,output 
    match reg, param, output with
    | UNKNOWN, UNKNOWN, _ -> skip 
    | UNKNOWN, _, UNKNOWN -> skip 
    | _ -> failwith $"narrowAdd: Not implemented: {reg} {param} {output}"

let narrowMul (reg: Value) (param: Value) (output: Value) : Value * Value * Value =
    let skip = reg,param,output 
    match reg, param, output with
    | UNKNOWN, UNKNOWN, UNKNOWN -> skip
    | _ -> failwith $"narrowMul: Not implemented: {reg} {param} {output}"

let narrowDiv (reg: Value) (param: int64) (output: Value) : Value * Value =
    let skip = reg,output 
    match reg, param, output with
    | UNKNOWN, _, UNKNOWN -> skip
    | _ -> failwith $"narrowDiv: Not implemented: {reg} {param} {output}"

let narrowMod (reg: Value) (param: int64) (output: Value) : Value * Value =
    let outputRange =
        SourcedValue.ofInts ([ 0 .. (param |> int) - 1 ])
        |> VALUES
    match reg, param, output with
    | UNKNOWN, _, UNKNOWN -> UNKNOWN, outputRange
    | _ -> failwith $"narrowMod: Not implemented: {reg} {param} {output}"

let narrowEql (reg: Value) (param: Value) (output: Value) : Value * Value * Value =
    match reg, param, output with
    | UNKNOWN, _, UNKNOWN -> UNKNOWN, param, VALUES binary
    | _ -> failwith $"narrowEql: Not implemented: {reg} {param} {output}"

type Step(inst: Inst, input: ALU, output: ALU) =
    member this.Inst = inst
    member this.Input = input
    member this.Output = output

    member this.NarrowValues() =
        match inst with
        | NOP ->
            this 
        | INP (r, value) ->
            let value = intersect (output.Get r) value
            let inst = INP(r, value)
            let output = output.Set r value
            let input = input.Set r VOID
            Step(inst, input, output)
        | SET (r, value) ->
            let value = intersect (output.Get r) (value)
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

    override this.ToString() =
        $"⍗ {inst |> inst2String, -15} IN:{input, -30}   OUT:{output, -30}"

let instructionsToProgram (instructions: list<Inst>) =
    let rec convert (instructions: list<Inst>) =
        match instructions with
        | [] -> []
        | [ inst ] -> [ Step(inst, ALU.unknown, ALU.final) ]
        | inst :: rest ->
            Step(inst, ALU.unknown, ALU.unknown)
            :: convert (rest)

    let program = convert instructions
    let first = program.Head

    let first =
        Step(first.Inst, ALU.initial, first.Output)

    first :: program.Tail

let readInput (filename: string) : List<Step> =
    readInstructions filename |> instructionsToProgram

let program = readInput "input.txt"

let solve (program: List<Step>) =
    program |> List.map (fun s -> s.NarrowValues())

program |> solve |> List.map (printfn "%A")
