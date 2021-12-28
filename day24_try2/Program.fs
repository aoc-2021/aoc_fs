open System.IO

let file =
    File.ReadAllLines "input.txt" |> Array.toList

file |> List.map (fun f -> printfn $"{f}")

type Op =
    | INP
    | ADD
    | MUL
    | DIV
    | MOD
    | EQL
    | Xset
    | XsetR

type Reg =
    | W
    | X
    | Y
    | Z

type Value =
    | R of Reg
    | I of int64
    | NA

type Instr =
    | Inst of Op * Reg * Value
    member this.Op =
        match this with
        | Inst (op, _, _) -> op

    member this.Reg =
        match this with
        | Inst (_, reg, _) -> reg

    member this.Value =
        match this with
        | Inst (_, _, value) -> value

    override this.ToString() =
        match this with
        | Inst (op, reg, value) ->
            let vString =
                match this.Value with
                | R reg -> reg.ToString()
                | I i -> i.ToString()
                | NA -> ""

            $"{op} {reg} {vString}"

type Program = list<Instr>

let parseLine (line: string) =
    let toOp (s: string) =
        match s with
        | "inp" -> INP
        | "add" -> ADD
        | "mul" -> MUL
        | "div" -> DIV
        | "mod" -> MOD
        | "eql" -> EQL
        | _ -> failwith $"unknown instruction {s}"

    let toReg reg =
        match reg with
        | "w" -> W
        | "x" -> X
        | "y" -> Y
        | "z" -> Z

    let toValue (v: string) =
        let c = v.Chars 0

        if c >= 'a' && c <= 'z' then
            R(toReg v)
        else
            I(v |> int64)

    let line = line.Split ' '
    let op = toOp line.[0]
    let reg = toReg line.[1]

    let value =
        if line.Length > 2 then
            toValue line.[2]
        else
            NA

    Inst(op, reg, value)

let program = file |> List.map parseLine

program |> List.map (printfn "%A")

let rec constElim (program: Program) : Program =
    let rec elim (state: Map<Reg, int64>) (program: Program) : Program =
        let isKnown = state.ContainsKey
        let isZero = state.TryFind >> ((=) (Some 0L))
        let isOne = state.TryFind >> ((=) (Some 1L))
        let get = state.TryFind >> Option.get

        match program with
        | [] -> []
        | inst :: rest ->
            // printfn $"elim {state} {inst}"
            match inst with
            | Inst (INP, reg, _) ->
                let state = state.Remove(reg)
                inst :: (elim state rest)
            | Inst (ADD, reg, R other) when isKnown other ->
                let inst = Inst(ADD, reg, I(get other))
                elim state (inst :: rest)
            | Inst (ADD, _, I 0L) -> elim state rest
            | Inst (ADD, reg, I i) when isKnown reg ->
                let value = (get reg) + i
                let inst = Inst(Xset, reg, I value)
                let state = state.Add(reg, value)
                inst :: (elim state rest)
            | Inst (ADD, reg, I _) ->
                let state = state.Remove(reg)
                inst :: (elim state rest)
            | Inst (ADD, reg, R other) when isZero reg ->
                let inst = Inst(XsetR, reg, R other)
                let state = state.Remove(reg)
                inst :: (elim state rest)
            | Inst (ADD, reg, R _) ->
                let state = state.Remove(reg)
                inst :: (elim state rest)
            | Inst (MUL, reg, R other) when isKnown other ->
                let inst = Inst(MUL, reg, I(get other))
                elim state (inst :: rest)
            | Inst (MUL, reg, I 0L) when isZero reg -> elim state rest
            | Inst (MUL, reg, I 0L) ->
                let inst = Inst(Xset, reg, I 0L)
                let state = state.Add(reg, 0L)
                inst :: (elim state rest)
            | Inst (MUL, _, I 1L) -> elim state rest
            | Inst (MUL, reg, I i) when isKnown reg ->
                let value = (get reg) * i
                let inst = Inst(Xset, reg, I value)
                let state = state.Add(reg, value)
                inst :: (elim state rest)
            | Inst (MUL, reg, R _) when isZero reg -> elim state rest
            | Inst (MUL, reg, R other) when isOne reg ->
                let inst = Inst(XsetR, reg, R other)
                let state = state.Remove(reg)
                inst :: (elim state rest)
            | Inst (MUL, reg, R _) ->
                let state = state.Remove(reg)
                inst :: (elim state rest)
            | Inst (DIV, reg, R other) when isKnown other ->
                let inst = Inst(DIV, reg, I(get other))
                elim state (inst :: rest)
            | Inst (DIV, _, I 1L) -> elim state rest
            | Inst (DIV, reg, I i) when isKnown reg ->
                let value = (get reg) / i
                let inst = Inst(Xset, reg, I value)
                let state = state.Add(reg, value)
                inst :: (elim state rest)
            | Inst (DIV, reg, I i) -> inst :: (elim state rest)
            | Inst (DIV, reg, R _) when isZero reg -> inst :: (elim state rest)
            | Inst (DIV, reg, R _) when isOne reg -> inst :: (elim state rest)
            | Inst (DIV, reg, R _) ->
                let state = state.Remove(reg)
                inst :: (elim state rest)
            | Inst (MOD, reg, R other) when isKnown other ->
                let inst = Inst(MOD, reg, I(get other))
                elim state (inst :: rest)
            | Inst (MOD, reg, I i) when isKnown reg ->
                let value = (get reg) % i
                let inst = Inst(Xset, reg, I value)
                let state = state.Add(reg, value)
                inst :: (elim state rest)
            | Inst (MOD, _, I _) -> inst :: (elim state rest)
            | Inst (MOD, reg, R _) when isZero reg -> inst :: (elim state rest)
            | Inst (MOD, reg, R _) when isOne reg -> inst :: (elim state rest)
            | Inst (MOD, reg, R _) ->
                let state = state.Remove(reg)
                inst :: (elim state rest)
            | Inst (EQL, reg, R other) when isKnown other ->
                let inst = Inst(EQL, reg, I(get other))
                elim state (inst :: rest)
            | Inst (EQL, reg, I i) when isKnown reg ->
                let value = if (get reg) = i then 1L else 0L
                let inst = Inst(Xset, reg, I value)
                let state = state.Add(reg, value)
                inst :: (elim state rest)
            | Inst (EQL, _, I _) -> inst :: (elim state rest)
            | Inst (EQL, reg, R _) ->
                let state = state.Remove(reg)
                inst :: (elim state rest)
            | inst ->
                printfn $"Unhandled instruction: {inst} [corrupt]"
                inst :: (elim state rest)

    let initState =
        [ (W, 0L); (X, 0L); (Y, 0L); (Z, 0L) ] |> Map

    elim initState program

let program1 = constElim program

printfn $"{program.Length} -> {program1.Length}"

printfn "Removed constants: "
program1 |> List.map (printfn "%A")

// constants gone, lets continue

type QValue =
    | TooMany
    | Vals of Set<int64>
    static member input = [ 1L .. 9L ] |> Set |> Vals
    static member zero = Set.singleton 1L |> Vals

    static member singleton(i: int64) = Vals(Set.singleton i)

    member this.IsSingle =
        match this with
        | TooMany -> false
        | Vals v -> v.Count = 1

    member this.GetSingleValue() =
        match this with
        | Vals v when v.Count = 1 -> v |> Set.toSeq |> Seq.head
        | _ -> failwith "Tried to get a single value from a quantum state"

    member this.Add(i: int64) =
        match this with
        | TooMany -> TooMany
        | Vals vals -> vals |> Set.map (fun v -> v + i) |> Vals

    member this.QAdd(other: QValue) =
        match (this, other) with
        | TooMany, _ -> TooMany
        | _, TooMany -> TooMany
        | Vals v1, Vals v2 ->
            if v1.Count * v2.Count > 1000 then
                TooMany
            else
                let v1 = v1 |> Set.toList
                let v2 = v2 |> Set.toList
                let pairs = List.allPairs v1 v2
                let sums = pairs |> List.map (fun (a, b) -> a + b)
                Vals(sums |> Set)

    member this.Mul(i: int64) =
        match this with
        | TooMany -> TooMany
        | Vals v1 -> v1 |> Set.map (fun v -> v * i) |> Vals

    member this.QMul(other: QValue) =
        match (this, other) with
        | TooMany, _ -> TooMany
        | _, TooMany -> TooMany
        | Vals v1, Vals v2 ->
            if v1.Count * v2.Count > 1000 then
                TooMany
            else
                let v1 = v1 |> Set.toList
                let v2 = v2 |> Set.toList
                let pairs = List.allPairs v1 v2
                let products = pairs |> List.map (fun (a, b) -> a * b)
                Vals(products |> Set)

    member this.Div(i: int64) =
        match this with
        | TooMany -> TooMany
        | Vals v1 -> v1 |> Set.map (fun v -> v / i) |> Vals

    member this.QDiv(other: QValue) =
        match (this, other) with
        | TooMany, _ -> TooMany
        | _, TooMany -> TooMany
        | Vals v1, Vals v2 ->
            let v2 = v2 |> Set.filter (fun v -> v <> 0L)

            if v1.Count * v2.Count > 1000 then
                TooMany
            else
                let v1 = v1 |> Set.toList
                let v2 = v2 |> Set.toList
                let pairs = List.allPairs v1 v2
                let divs = pairs |> List.map (fun (a, b) -> a / b)
                Vals(divs |> Set)

    member this.Mod(i: int64) =
        match this with
        | TooMany -> TooMany
        | Vals v1 ->
            let v1 = v1 |> Set.filter (fun v -> v >= 0L)
            v1 |> Set.map (fun v -> v / i) |> Vals

    member this.QMod(other: QValue) =
        match (this, other) with
        | TooMany, _ -> TooMany
        | _, TooMany -> TooMany
        | Vals v1, Vals v2 ->
            let v1 = v1 |> Set.filter (fun v -> v >= 0L)
            let v2 = v2 |> Set.filter (fun v -> v > 0L)

            if v1.Count * v2.Count > 10000 then
                TooMany
            else
                let v1 = v1 |> Set.toList
                let v2 = v2 |> Set.toList
                let pairs = List.allPairs v1 v2
                let mods = pairs |> List.map (fun (a, b) -> a % b)
                Vals(mods |> Set)

    member this.Eq(i: int64) =
        match this with
        | TooMany -> [ 0L; 1L ] |> Set |> Vals
        | Vals v1 ->
            v1
            |> Set.map (fun v -> if v = i then 1L else 0L)
            |> Vals

    member this.QEq(other: QValue) =
        match this, other with
        | TooMany, _ -> [ 0L; 1L ] |> Set |> Vals
        | _, TooMany -> [ 0L; 1L ] |> Set |> Vals
        | Vals v1, Vals v2 ->
            let v1 = v1 |> Set.toList
            let v2 = v2 |> Set.toList

            List.allPairs v1 v2
            |> List.map (fun (a, b) -> if a = b then 1L else 0L)
            |> Set
            |> Vals

    member this.Filter(filter: int64 -> bool) =
        match this with
        | TooMany -> TooMany
        | Vals v1 -> v1 |> Set.filter filter |> Vals

    override this.ToString() =
        let values =
            match this with
            | TooMany -> "∞"
            | Vals v ->
                v
                |> Set.toList
                |> List.map string
                |> String.concat " "

        $"{{{values}}}"

type QState = Map<Reg, QValue>

let quantumOptimize (program: Program) =
    let rec eval (state: QState) (program: Program) =
        let isVals (reg: Reg) : bool =
            match state.TryFind reg |> Option.get with
            | Vals _ -> true
            | TooMany -> false

        let isInfinite (reg: Reg) : bool =
            match state.TryFind reg |> Option.get with
            | Vals _ -> false
            | TooMany -> true

        let isSingle (reg: Reg) =
            state.TryFind reg
            |> Option.get
            |> (fun v -> v.IsSingle)

        let getSingle =
            state.TryFind
            >> Option.get
            >> (fun v -> v.GetSingleValue())

        let get = state.TryFind >> Option.get

        match program with
        | [] -> []
        | inst :: rest ->
            match inst with
            | Inst (ins, reg, R other) when isSingle other ->
                let value = get other
                let value = value.GetSingleValue()
                let inst = Inst(ins, reg, I value)
                eval state (inst :: rest)
            | Inst (INP, reg, _) ->
                let state = state.Add(reg, QValue.input)
                inst :: (eval state rest)
            | Inst (ADD, reg, I value) when isSingle reg ->
                let value = (getSingle reg) + value
                let inst = Inst(Xset, reg, I value)
                let state = state.Add(reg, QValue.singleton value)
                inst :: (eval state rest)
            | Inst (ADD, reg, I value) ->
                let regv = reg |> get
                let regv = regv.Add value
                let state = state.Add(reg, regv)
                inst :: (eval state rest)
            | Inst (ADD, reg, R other) ->
                let v1 = get reg
                let v2 = get other
                let value = v1.QAdd v2
                let state = state.Add(reg, value)
                inst :: (eval state rest)
            | Inst (MUL, reg, I value) when isSingle reg ->
                let value = (getSingle reg) * value
                let inst = Inst(Xset, reg, I value)
                let state = state.Add(reg, QValue.singleton value)
                inst :: (eval state rest)
            | Inst (MUL, reg, I value) ->
                let regv = get reg
                let regv = regv.Mul value
                let state = state.Add(reg, regv)
                inst :: (eval state rest)
            | Inst (MUL, reg, R other) ->
                let v1 = get reg
                let v2 = get other
                let value = v1.QMul v2
                let state = state.Add(reg, value)
                inst :: (eval state rest)
            | Inst (DIV, reg, I i) when isSingle reg ->
                let value = (getSingle reg) / i
                let inst = Inst(Xset, reg, I value)
                let state = state.Add(reg, (QValue.singleton value))
                inst :: (eval state rest)
            | Inst (DIV, reg, I i) ->
                let regv = get reg
                let regv = regv.Div i
                let state = state.Add(reg, regv)
                inst :: (eval state rest)
            | Inst (DIV, reg, R other) ->
                let v1 = get reg
                let v2 = get other
                let v2 = v2.Filter((<>) 0L)
                let value = v1.QDiv v2
                let state = state.Add(reg, value).Add(other, v2)
                inst :: (eval state rest)
            | Inst (MOD, reg, I i) when isSingle reg ->
                let value = (getSingle reg) % i
                let inst = Inst(Xset, reg, I value)
                let state = state.Add(reg, (QValue.singleton value))
                inst :: (eval state rest)
            | Inst (MOD, reg, I i) ->
                let regv = get reg
                let regv = regv.Filter((<=) 0L)
                let regv = regv.Mod i
                let state = state.Add(reg, regv)
                inst :: (eval state rest)
            | Inst (MOD, reg, R other) ->
                let v1 = get reg
                let v1 = v1.Filter((<=) 0L)
                let v2 = get other
                let v2 = v2.Filter((<) 0L)
                let value = v1.QMod v2
                let state = state.Add(reg, value).Add(other, v2)
                inst :: (eval state rest)
            | Inst (EQL, reg, I i) ->
                let value = get reg
                let value = value.Eq i

                if value.IsSingle then
                    let inst =
                        Inst(Xset, reg, I(value.GetSingleValue()))

                    let state = state.Add(reg, value)
                    inst :: (eval state rest)
                else
                    let state = state.Add(reg, value)
                    inst :: (eval state rest)
            | Inst (EQL, reg, R other) ->
                let v1 = get reg
                let v2 = get other
                let value = v1.QEq v2

                if value.IsSingle then
                    let inst =
                        Inst(Xset, reg, I(value.GetSingleValue()))

                    let state = state.Add(reg, value)
                    inst :: (eval state rest)
                else
                    let state = state.Add(reg, value)
                    inst :: (eval state rest)
            | Inst (Xset, reg, I i) ->
                let state = state.Add(reg, QValue.singleton i)
                inst :: (eval state rest)
            | Inst (XsetR, reg, R other) ->
                let state = state.Add(reg, get other)
                inst :: (eval state rest)
            | _ ->
                printfn $"Not implemented: {inst} [corrupt]"
                inst :: (eval state rest)

    let initState: QState =
        [ (Z, QValue.zero)
          (X, QValue.zero)
          (Y, QValue.zero)
          (Z, QValue.zero) ]
        |> Map

    eval initState program

let program2 = quantumOptimize program1

printfn ""
printfn "** Quantum optimized: **"
program2 |> List.map (printfn "%A")
