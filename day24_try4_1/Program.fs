open System.IO

printfn "Hello" 
let file =
    File.ReadAllLines "input.txt" |> Array.toList

type Reg =
    | W
    | X
    | Y
    | Z
type Assertion = 
    | EQ of int64
    | NEQ of int64
    | GT of int64
    | REQ of Reg 
type Inst =
    | INP of Reg
    | ADDI of Reg * int64
    | ADDR of Reg * Reg
    | MULI of Reg * int64
    | MULR of Reg * Reg
    | DIVI of Reg * int64
    | MODI of Reg * int64
    | EQLI of Reg * int64
    | EQLR of Reg * Reg
    | SETI of Reg * int64
    | SETR of Reg * Reg
    | ASSERT of Reg*Assertion 

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
    | [| "add"; reg; i |] -> ADDI(toReg reg, i |> int64)
    | [| "mul"; reg; reg2 |] when isReg reg2 -> MULR(toReg reg, toReg reg2)
    | [| "mul"; reg; i |] -> MULI(toReg reg, i |> int64)
//    | [| "div"; reg; reg2 |] when isReg reg2 -> DIVR(toReg reg, toReg reg2)
    | [| "div"; reg; i |] -> DIVI(toReg reg, i |> int64)
//    | [| "mod"; reg; reg2 |] when isReg reg2 -> MODR(toReg reg, toReg reg2)
    | [| "mod"; reg; i |] -> MODI(toReg reg, i |> int64)
    | [| "eql"; reg; reg2 |] when isReg reg2 -> EQLR(toReg reg, toReg reg2)
    | [| "eql"; reg; i |] -> EQLI(toReg reg, i |> int64)
    | _ -> failwith $"Unknown input: {line}"

let program = file |> List.map parseLine

printfn $"program = {program}"

let printProgram (program: Program) =
    printfn "Program:"
    program |> List.map (fun s -> printfn $"{s}")
    printfn ""

let rec simplify (program: Program) : Program =
    match program with
    | [] -> []
    | inst :: rest ->
        let rest = simplify rest

        match inst with
        | ADDI (_, 0L) -> rest
        | MULI (reg, 0L) -> SETI(reg, 0L) :: rest
        | MULI (reg, 1L) -> rest
        | _ -> inst :: rest

let assertFromBack (program:Program) : Program =
    let rec eval (known:Map<Reg,int64>) (program:Program) =
        let isKnown = known.ContainsKey
        let get = known.TryFind >> Option.get 
        match program with
        | [] -> []
        | inst :: rest ->
            match inst with
            | INP reg ->
                let known = known.Remove(reg)
                inst :: eval known rest
            | ADDI (reg,i) when isKnown reg ->
                let value = get reg - i
                let known = known.Add (reg,value)
                inst :: (eval known rest)
            | ADDI (reg,i) ->
                inst :: (eval known rest)
            | ADDR (reg,other) when isKnown reg && isKnown other -> 
                let value = (get reg) - (get other)
                let known = known.Add(reg,value)
                inst :: (eval known program)
            | ADDR (reg,other) when isKnown reg ->
                let check = ASSERT (reg,(EQ (get reg)))
                let known = known.Remove reg
                check :: inst :: (eval known rest)
            | ADDR (reg,other) ->
                inst :: (eval known rest)
            | MULI (reg,i) when isKnown reg ->
                let value = get reg / i
                let known = known.Add (reg,value)
                inst :: (eval known rest)
            | MULI (reg,i) ->
                inst :: (eval known rest)
            | MULR (reg,other) when isKnown reg && isKnown other ->
                match (get reg, get other) with
                | (_,0L) ->
                    let check = ASSERT (reg,(EQ 0L))
                    let known = known.Remove(reg)
                    check :: inst :: (eval known rest)
                | (v1,v2) ->
                    let value = v1 / v2
                    let known = known.Add(reg,value)
                    inst :: (eval known rest)
            | MULR (reg,_) when isKnown reg ->
                let check = ASSERT (reg,(EQ (get reg)))
                let known = known.Remove reg
                check :: inst :: (eval known rest)
            | MULR _ ->
                inst :: (eval known rest)
                
            | DIVI (reg,i) when isKnown reg ->
                let value = get reg * i
                let known = known.Add (reg,value)
                inst :: (eval known rest)
            | DIVI _ ->
                inst :: (eval known rest)
            | MODI (reg,_) when isKnown reg ->
                let check = ASSERT (reg,EQ (get reg))
                let known = known.Remove reg
                check :: inst :: (eval known rest)
            | MODI _ ->
                inst :: (eval known rest)
                           
            | EQLI (reg,value) when isKnown reg ->
                match get reg with
                | 0L ->
                    let check = ASSERT (reg,(NEQ value))
                    let known = known.Remove reg
                    check :: inst :: (eval known rest)
                | 1L ->
                    let known = known.Add(reg,value)
                    inst :: (eval known rest)
            | EQLI _ -> 
                inst :: (eval known rest)
            | EQLR (reg,other) when isKnown reg && isKnown other ->
                match (get reg,get other) with
                | 0L,v2 ->
                    let check = ASSERT (reg,NEQ(v2))
                    let known = known.Remove(reg)
                    check :: inst :: (eval known rest)
                | 1L,v2 ->
                    let known = known.Add(reg,v2)
                    inst :: (eval known rest)
            | EQLR (reg,other) when isKnown reg ->
                match (get reg) with
                | 0L ->
                    let known = known.Remove (reg)
                    inst :: (eval known rest)
                | 1L ->
                    let known = known.Remove (reg)
                    let check = ASSERT (reg,REQ other)
                    inst :: check :: (eval known rest)
            | EQLR _ -> 
                    inst :: (eval known rest)
                            
            | SETI (reg,_) ->
                let known = known.Remove reg
                inst :: (eval known rest)
            | SETR (reg,other) when isKnown reg ->
                let known = known.Add(other,get reg)
                inst :: (eval known rest)
            | _ ->
                printfn $"NOT IMPLEMENTED {inst}"
                inst :: (eval known rest)
    let known = [(Z,0L)] |> Map 
    program |> List.rev
            |> (eval known)
            |> List.rev 
                
let optimize (program: Program) =
    let program = simplify program
    printProgram program
    let program = assertFromBack program 
    printProgram program 
optimize program
