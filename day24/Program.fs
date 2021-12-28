open System.IO

let _9to1 = [ 1L .. 9L ] |> List.rev
let file = File.ReadAllLines "input.txt"

type Register =
    | W
    | X
    | Y
    | Z
    member this.Index =
        match this with
        | W -> 0
        | X -> 1
        | Y -> 2
        | Z -> 3

    static member ofString(s: string) : Register =
        match s with
        | "w" -> W
        | "x" -> X
        | "y" -> Y
        | "z" -> Z
        | "W" -> W
        | "X" -> X
        | "Y" -> Y
        | "Z" -> Z

type Value =
    | Reg of Register
    | Literal of int64

type Instruction =
    | Inp of Register
    | Add of Register * Value
    | Mul of Register * Value
    | Div of Register * Value
    | Mod of Register * Value
    | Eql of Register * Value
    | XSet of Register * int64
    | XSetR of Register * Register
    | XZero of Register
    | XNoop

type Program = list<Instruction>

type ALU(regs: Map<Register, int64>, inputs: int64) =
    let get (value: Value) : int64 =
        match value with
        | Literal v -> v
        | Reg reg -> regs.TryFind reg |> Option.defaultValue 0L

    let set (reg: Register) (value: int64) =
        let regs = regs.Add(reg, value)
        ALU(regs, inputs)

    member this.Set = set
    member this.Get = get

    member this.Inputs = inputs

    member this.Input (reg: Register) (value: int64) =
        let regs = regs.Add(reg, value)
        let inputs = inputs * 10L + value
        ALU(regs, inputs)

    member this.Execute(i: Instruction) : Option<ALU> =
        match i with
        | Inp _ -> failwith "Inp is not supported"
        | Add (reg, value) ->
            let a = get (Reg reg)
            let b = get value
            Some(set reg (a + b))
        | Mul (reg, value) ->
            let a = get (Reg reg)
            let b = get value
            Some(set reg (a * b))
        | Div (reg, value) ->
            let a = get (Reg reg)
            let b = get value

            if b = 0 then
                printfn $"Divide by Zero {reg}/{value}"
                None
            else
                Some(set reg (a / b))
        | Mod (reg, value) ->
            let a = get (Reg reg)
            let b = get value

            if a < 0L || b <= 0L then
                printfn $"Mod fail {reg}={a} {value}={b}"
                None
            else
                Some(set reg (a % b))
        | Eql (reg, value) ->
            let a = get (Reg reg)
            let b = get value
            Some(set reg (if a = b then 1L else 0L))
        | XSet (reg, value) -> Some(set reg value)
        | XSetR (reg, other) ->
            let value = get (Reg other)
            Some(set reg value)
        | XZero reg -> Some(set reg 0L)
        | XNoop -> Some(this)

    static member empty = ALU(Map.empty, 0L)

    override this.ToString() =
        let w = regs.TryFind W |> Option.defaultValue 0L
        let x = regs.TryFind X |> Option.defaultValue 0L
        let y = regs.TryFind Y |> Option.defaultValue 0L
        let z = regs.TryFind Z |> Option.defaultValue 0L
        $"ALU(W={w} X={x} Y={y} Z={z})"

let parseLine (line: string) =
    let toReg = Register.ofString

    let isReg (s: string) =
        s = "w"
        || s = "W"
        || s = "x"
        || s = "X"
        || s = "y"
        || s = "Y"
        || s = "z"
        || s = "Z"

    let toVal (s: string) =
        if isReg s then
            Reg(toReg s)
        else
            Literal(s |> int64)

    let line = line.Split ' '

    match line with
    | [| "inp"; reg |] -> Inp(toReg reg)
    | [| "add"; reg; value |] -> Add((toReg reg), (toVal value))
    | [| "mul"; reg; value |] -> Mul((toReg reg), (toVal value))
    | [| "div"; reg; value |] -> Div((toReg reg), (toVal value))
    | [| "mod"; reg; value |] -> Mod((toReg reg), (toVal value))
    | [| "eql"; reg; value |] -> Eql((toReg reg), (toVal value))
    | _ -> failwith $"Cannot parse {line}"

let parseFile (input: string []) : list<Instruction> =
    input |> Array.toList |> List.map parseLine

let prodProgram = (parseFile file)

let rec exec (alu: ALU) (program: Program) : Option<list<int64>> =
    let execWithInput (alu: ALU) (reg: Register) (value: int64) (rest: Program) : Option<list<int64>> =
        let alu = alu.Input reg value
        let result = exec alu rest

        result
        |> Option.map (fun digits -> value :: digits)

    match program with
    | [] ->
        // printfn $"ALU={alu}"
        if alu.Get(Reg Z) = 0L then
            Some([])
        else
            printfn $"Invalid Model Number, inputs = {alu.Inputs}"
            None
    | Inp reg :: rest ->
        _9to1
        |> List.toSeq
        |> Seq.map (fun n -> execWithInput alu reg n rest)
        |> Seq.tryFind Option.isSome
        |> Option.flatten
    | inst :: rest ->
        alu.Execute inst
        |> Option.map (fun alu -> exec alu rest)
        |> Option.flatten

let testProgram1 = [ Inp X; Mul(X, Literal -1L) ]

let testProgram2 =
    [ Inp Z
      Inp X
      Mul(Z, Literal 3L)
      Eql(Z, Reg X) ]

let testProgram3 =
    [ Inp W
      Add(Z, (Reg W))
      Mod(Z, Literal 2L)
      Div(W, Literal 2L)
      Add(Y, Reg W)
      Mod(Y, Literal 2L)
      Div(W, Literal 2L)
      Add(X, Reg W)
      Mod(X, Literal 2L)
      Div(W, Literal 2L)
      Mod(W, Literal 2L) ]

let myTestProgram4 =
    [ Inp X
      Inp Y
      Inp Z
      Mod(X, Literal 3L)
      Add(Y, Literal -1L)
      Mod(Z, Literal 3L)
      Div(Y, Reg X)
      Div(Y, Reg Z) ]

let testProgram5 =
    [ Add(X, Literal -5L)
      Add(Y, Literal 0L)
      Div(X, Reg Y) ]

type OptimizerState(regs: Map<Register, int64>) =
    member this.Regs = regs
    member this.SetDirty(reg: Register) : OptimizerState = OptimizerState(regs.Remove reg)
    member this.IsDirty: Register -> bool = regs.ContainsKey >> not
    member this.IsClean = this.IsDirty >> not

    member this.GetValue = regs.TryFind

    member this.IsZero reg = regs.TryFind reg = Some(0L)
    member this.IsOne reg = regs.TryFind reg = Some(1L)

    member this.SetConst (reg: Register) (value: int64) : OptimizerState = OptimizerState(regs.Add(reg, value))

    static member fresh =
        [ (W, 0L); (X, 0L); (Y, 0L); (Z, 0L) ]
        |> Map
        |> OptimizerState

type OptimizerConstElim(state: OptimizerState) =
    let dirty = state.SetDirty >> OptimizerConstElim
    let isDirty = state.IsDirty
    let isKnown = state.IsDirty >> not
    let setDirty = state.SetDirty >> OptimizerConstElim
    let getKnown = state.GetValue >> Option.get
    let isZero = state.IsZero
    let isOne = state.IsOne

    let setConst reg value =
        state.SetConst reg value |> OptimizerConstElim

    member this.State = state

    member this.MapConst(inst: Instruction) : Instruction * OptimizerConstElim =
        match inst with
        | Inp reg -> inst, (dirty reg)
        | Add (_, Literal 0L) -> XNoop, this
        | Add (reg, Literal _) when isDirty reg -> inst, this
        | Add (reg, Literal i) ->
            let value = getKnown reg + i
            let optimizer = setConst reg value
            let inst = XSet(reg, value)
            inst, optimizer
        | Add (reg, Reg other) when isKnown other ->
            let inst = Add(reg, Literal(getKnown other))
            this.MapConst inst
        | Add (reg, Reg other) when isDirty other -> inst, setDirty reg
        | Add (reg, Reg other) when isDirty reg -> Add(reg, Literal(getKnown other)), this
        | Add (reg, Reg other) ->
            let value = getKnown reg + getKnown other
            XSet(reg, value), setConst reg value
        | Mul (reg, Literal 0L) when isDirty reg -> XZero reg, setConst reg 0L
        | Mul (reg, Literal 0L) when getKnown reg = 0L -> XNoop, this
        | Mul (reg, Literal 0L) -> XZero reg, setConst reg 0L
        | Mul (_, Literal 1L) -> XNoop, this
        | Mul (reg, Literal n) when isKnown reg ->
            let value = getKnown reg * n
            XSet(reg, value), setConst reg value
        | Mul (_, Literal _) -> inst, this
        | Mul (reg, Reg other) when isKnown other ->
            let inst = Mul(reg, Literal(getKnown other))
            this.MapConst inst
        | Mul (reg, Reg _) when isDirty reg -> inst, this
        | Mul (reg, _) when isZero reg -> XNoop, this
        | Mul (reg, Reg other) when isOne reg -> XSetR(reg, other), setDirty reg
        | Mul (reg, Reg _) -> inst, setDirty reg
        | Div (_, Literal 1L) -> XNoop, this
        | Div (reg, Literal n) when isKnown reg ->
            let value = getKnown reg / n
            XSet(reg, value), setConst reg value
        | Div (_, Literal _) -> inst, this
        | Div (reg, Reg other) when isKnown other ->
            let inst = Div(reg, Literal(getKnown other))
            this.MapConst inst
        | Div (reg, Reg _) -> inst, setDirty reg
        | Mod (reg, Literal n) when isKnown reg ->
            let value = getKnown reg % n
            XSet(reg, value), setConst reg value
        | Mod (_, Literal _) -> inst, this
        | Mod (reg, Reg other) when isKnown other ->
            let inst = Mod(reg, Literal(getKnown other))
            this.MapConst inst
        | Mod (reg, Reg _) -> inst, setDirty reg
        | Eql (reg, Literal n) when isKnown reg ->
            let value = if getKnown reg = n then 1L else 0L
            XSet(reg, value), setConst reg value
        | Eql (_, Literal _) -> inst, this
        | Eql (reg, Reg other) when isKnown other ->
            let inst = Eql(reg, Literal(getKnown other))
            this.MapConst inst
        | Eql (reg, Reg _) -> inst, setDirty reg

        | _ ->
            printfn $"Not handled: {inst} [state corrupt]"
            inst, this

    static member fresh =
        OptimizerState.fresh |> OptimizerConstElim

let eliminateConstants (program: Program) : Program =
    let rec optimize (optimizer: OptimizerConstElim) (program: Program) =
        match program with
        | [] -> []
        | inst :: rest ->
            let inst, optimizer = optimizer.MapConst inst
            inst :: (optimize optimizer rest)

    optimize OptimizerConstElim.fresh program


type OptimizerNoopRemoval() =
    let isNoop (inst: Instruction) =
        match inst with
        | XNoop -> true
        | Add (_, Literal 0L) -> true
        | Mul (_, Literal 1L) -> true
        | _ -> false

    member this.Filter(program: Program) : Program = program |> List.filter (isNoop >> not)

let cleanup = OptimizerNoopRemoval().Filter


type OptimizerDeadCodeElim() =
    let isInstructionUsed (inst: Instruction) (used: Set<Register>) : bool * Set<Register> =
        match inst with
        | Inp reg -> true, used.Remove reg
        | Add (reg, Literal _) -> used.Contains reg, used
        | Add (reg, Reg other) ->
            if used.Contains reg then
                true, used.Add(other)
            else
                false, used
        | Mul (reg, Literal _) -> used.Contains reg, used
        | Mul (reg, Reg other) ->
            if used.Contains reg then
                true, used.Add(other)
            else
                false, used
        | Div (reg, Literal _) -> used.Contains reg, used
        | Div (reg, Reg other) -> true, used.Add(reg).Add(other)
        | Mod (reg, Literal _) -> true, used.Add(reg)
        | Mod (reg, Reg other) -> true, used.Add(reg).Add(other)
        | Eql (reg, Literal _) -> used.Contains reg, used
        | Eql (reg, Reg other) ->
            if used.Contains reg then
                true, used.Add(other)
            else
                false, used
        | XSet (reg, value) -> used.Contains(reg), used.Remove reg
        | XSetR (reg, other) ->
            if used.Contains(reg) then
                true, used.Remove(reg).Add(other)
            else
                false, used
        | XZero reg -> used.Contains(reg), used.Remove(reg)
        | XNoop -> false, used


    member this.UnusedToNoops(program: Program) =
        let rec ``process`` (used: Set<Register>) (program: Program) : Program =
            match program with
            | [] -> []
            | inst :: rest ->
                let inUse, used = isInstructionUsed inst used
                let inst = if inUse then inst else XNoop
                inst :: (``process`` used rest)

        let program = program |> List.rev
        let init = Set.empty.Add(Z) // Z is used as the final eval
        let program = ``process`` init program
        let program = program |> List.rev
        program

let eliminateDeadCode: Program -> Program = OptimizerDeadCodeElim().UnusedToNoops


let program = prodProgram

let optimize (program: Program) : Program =
    program
    |> eliminateConstants
    |> cleanup
    |> eliminateDeadCode
    |> cleanup

let program1 = program |> optimize

program1 |> List.map (fun p -> printfn $"{p}")

printfn $"optimized: {program.Length} -> {program1.Length}"

// let result = exec ALU.empty program
// printfn $"result = {result}"

type Arit =
    | AInput of int64
    | AConst of int64
    | AAdd of Arit * Arit
    | AMul of Arit * Arit
    | ADiv of Arit * Arit
    | AMod of Arit * Arit
    | AEq of Arit * Arit

let toArit (program: Program) : Arit =
    let rec evalInstructions (regs: Map<Register, Arit>) (program: Program) (inputCounter: int64) : Arit =
        let lookup (reg: Register) =
            regs.TryFind(reg)
            |> Option.defaultValue (AConst 0L)

        let valueOf (value: Value) =
            match value with
            | Literal i -> AConst i
            | Reg reg -> lookup reg

        match program with
        | [] -> lookup Z 
        | Inp (reg) :: rest ->
            let regs = regs.Add(reg, AInput inputCounter)
            let inputCounter = inputCounter + 1L
            evalInstructions regs rest inputCounter
        | Add (reg, value) :: rest ->
            let regs =
                regs.Add(reg, AAdd(lookup reg, valueOf value))
            evalInstructions regs rest inputCounter
        | Mul (reg, value) :: rest ->
            let regs =
                regs.Add(reg, AMul(lookup reg, valueOf value))

            evalInstructions regs rest inputCounter
        | Div (reg, value) :: rest ->
            let regs =
                regs.Add(reg, ADiv(lookup reg, valueOf value))

            evalInstructions regs rest inputCounter
        | Mod (reg, value) :: rest ->
            let regs =
                regs.Add(reg, AMod(lookup reg, valueOf value))

            evalInstructions regs rest inputCounter
        | Eql (reg, value) :: rest ->
            let regs =
                regs.Add(reg, AEq(lookup reg, valueOf value))

            evalInstructions regs rest inputCounter
        | XSet (reg, i) :: rest ->
            let regs = regs.Add(reg, AConst i)
            evalInstructions regs rest inputCounter
        | XSetR (reg, other) :: rest ->
            let regs = regs.Add(reg, lookup other)
            evalInstructions regs rest inputCounter
        | XZero (reg) :: rest ->
            let regs = regs.Add(reg, AConst 0L)
            evalInstructions regs rest inputCounter
        | XNoop :: rest -> evalInstructions regs rest inputCounter

    evalInstructions Map.empty program 0L

let expression = toArit program1

let rec aritConstElim (expr:Arit) : Arit =
    match expr with
    | AInput inp -> AInput inp 
    | AConst inp -> AConst inp 
    | AAdd (a,b) ->
        let a = aritConstElim a
        let b = aritConstElim b
        match (a,b) with
        | AConst a,AConst b -> AConst (a+b)
        | AConst 0L,b -> b
        | a, AConst 0L -> a
        | a,b -> AAdd (a,b)
    | AMul (a,b) ->
        let a = aritConstElim a
        let b = aritConstElim b
        match (a,b) with
        | AConst a,AConst b -> AConst (a*b)
        | AConst 1L, b -> b
        | AConst 0L, _ -> AConst 0L 
        | a, AConst 1L -> a
        | _, AConst 0L -> AConst 0L 
        | a,b -> AMul (a,b)
    | ADiv (a,b) -> 
        let a = aritConstElim a
        let b = aritConstElim b
        match (a,b) with
        | AConst a,AConst b -> AConst (a/b)
        | AConst 0L,_ -> AConst 0L  // todo: must add a div check
        | a,b -> ADiv (a,b)
    | AMod (a,b) ->
        let a = aritConstElim a
        let b = aritConstElim b
        match (a,b) with
        | AConst a,AConst b -> AConst (a%b)
        | AInput n,AConst b when b > 10 -> AInput n
        | AConst 1L,_ -> AConst 1L // todo: must add a div check
        | AConst 0L,_ -> AConst 0L // todo: must add a div check
        | a,b -> AMod (a,b)
    | AEq (a,b) ->
        let a = aritConstElim a
        let b = aritConstElim b
        match (a,b) with
        | AConst a,AConst b -> AConst (if a = b then 1L else 0L)
        | AConst a, AInput _ when a > 9L || a < 0L -> AConst 0L
        | AInput _,AConst b when b > 9L || b < 0L -> AConst 0L 
        | a,b -> AEq (a,b)

let rec pushMulDown (arit:Arit) : Arit =
    printf "."
    match arit with
    | AInput a -> AInput a 
    | AConst c -> AConst c
    | AAdd (a,b) -> AAdd ((pushMulDown a),pushMulDown b)
    | AMul (AAdd (a,b),c) -> AAdd (pushMulDown (AMul (a,c)),pushMulDown (AMul(b,c)))
    | AMul (a,AAdd (b,c)) -> AAdd (pushMulDown (AMul (a,b)),pushMulDown (AMul (a,c)))
    | AMul (a,b) -> AMul (pushMulDown a,pushMulDown b)
    | ADiv (a,b) -> ADiv (pushMulDown a,pushMulDown b)
    | AMod (a,b) -> AMod (pushMulDown a,pushMulDown b)
    | AEq (a,b) -> AEq (pushMulDown a,pushMulDown b)


let rec findInputs (arit:Arit) : Set<Arit> =
    match arit with
    | AInput a -> Set.singleton(AInput a)
    | AConst _ -> Set.empty 
    | AAdd (a,b) -> Set.union (findInputs a) (findInputs b)
    | AMul (a,b) -> Set.union (findInputs a) (findInputs b)
    | ADiv (a,b) -> Set.union (findInputs a) (findInputs b)
    | AMod (a,b) -> Set.union (findInputs a) (findInputs b)
    | AEq (a,b) -> Set.union (findInputs a) (findInputs b)

let rec aritSize (arit:Arit) : int64 =
    match arit with
    | AInput a -> 1L
    | AConst _ -> 1L 
    | AAdd (a,b) -> 1L + (aritSize a) + (aritSize b) 
    | AMul (a,b) -> 1L + (aritSize a) + (aritSize b) 
    | ADiv (a,b) -> 1L + (aritSize a) + (aritSize b) 
    | AMod (a,b) -> 1L + (aritSize a) + (aritSize b) 
    | AEq (a,b) -> 1L + (aritSize a) + (aritSize b) 

let rec constMulAddElim (arit:Arit) : Arit =
    match arit with
    | AInput _ -> arit 
    | AConst _ -> arit
    | AAdd (a,b) -> AAdd(constMulAddElim a, constMulAddElim b)
    | AMul (AAdd (AConst a,b),AConst c) -> AAdd (AConst (a*c),AMul(b,AConst c))
    | AMul (AAdd (a,AConst b),AConst c) -> AAdd (AConst (b*c),AMul(a,AConst c))
    | AMul (AConst a,AAdd (AConst b,c)) -> AAdd (AConst (a*b),AMul(AConst a,c))
    | AMul (AConst a,AAdd (b,AConst c)) -> AAdd (AConst (a*c),AMul(AConst a,b))
    | AMul (a,b) -> AMul (constMulAddElim a,constMulAddElim b)
    | ADiv (a,b) -> ADiv (constMulAddElim a,constMulAddElim b)
    | AMod (a,b) -> AMod (constMulAddElim a,constMulAddElim b)
    | AEq (a,b) -> AEq (constMulAddElim a,constMulAddElim b)
    

let expression1 =
    expression
    |> aritConstElim

let expression2 =
    expression1
    |> constMulAddElim


printfn $"arit expression(1) = {expression1}"

let aritSize1 = aritSize expression
let aritSize2 = aritSize expression1
let aritSize3 = aritSize expression2

let aritSize4 = -1

printfn $"aritSize {aritSize1} -> {aritSize2} -> {aritSize3} -> {aritSize4}"

// let inputs = findInputs expression1
// inputs |> Set.toList |> List.map (fun f -> printfn $"{f}")