open System.IO

let file =
    File.ReadAllLines "input.txt" |> Array.toList

type Reg =
    | W
    | X
    | Y
    | Z



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
    | [| "inp"; reg |] -> INP (toReg reg)
    | [| "add"; reg ; reg2 |] when isReg reg2 -> ADDR (toReg reg, toReg reg2)
    | [| "add"; reg ; i |] -> ADDI (toReg reg,i |> int64)
    | [| "mul"; reg ; reg2 |] when isReg reg2 -> MULR (toReg reg, toReg reg2)
    | [| "mul"; reg ; i |] -> MULI (toReg reg,i |> int64)
    | [| "div"; reg ; reg2 |] when isReg reg2 -> DIVR (toReg reg, toReg reg2)
    | [| "div"; reg ; i |] -> DIVI (toReg reg,i |> int64)
    | [| "mod"; reg ; reg2 |] when isReg reg2 -> MODR (toReg reg, toReg reg2)
    | [| "mod"; reg ; i |] -> MODI (toReg reg,i |> int64)
    | [| "eql"; reg ; reg2 |] when isReg reg2 -> EQLR (toReg reg, toReg reg2)
    | [| "eql"; reg ; i |] -> EQLI (toReg reg,i |> int64)
    | _ -> failwith $"Unknown input: {line}"

let program = file |> List.map parseLine


// ok, lets try reverse again

let reverseProgram (program:Program) : Program =
    let reverseInstruction (inst:Inst) =
        match inst with
        | INP reg -> OUTP reg
        | ADDI (reg,i) -> ADDI (reg,-i)
        | ADDR (reg,other) -> SUBR (other,reg)
        | MULI (reg,i) -> DIVI (reg,i)
        | MULR (reg,r) -> DIVR (reg,r)
        | DIVI (reg,i) -> MULI (reg,i)
        | DIVR (reg,r) -> MULR (reg,r)
        | MODI (reg,i) -> RMODI (reg,i)
        | MODR (reg,r) -> failwith $"Not implemented: {inst}"
        | EQLI (reg,i) -> REQI (reg,i)
        | EQLR (reg,r) -> REQR (reg,r)
        | _ -> failwith $"Not implemented: {inst}"
    program |> List.rev |> List.map reverseInstruction 
                    
let rprogram = reverseProgram program 

printfn "Reversed: "
rprogram |> List.map (printfn "%A")

let rFind (program:Program) =
    let rec exec (regs:Map<Reg,int64>) (program:Program) : list<uint64> =
        match program with
        | [] -> []
        | inst::rest ->
            match inst with
            | _ -> 
                printfn $"Not implemented: {inst}"
                exec regs rest
    let startInst = SET (Z,0L)
    let program = startInst :: program
    exec Map.empty program

let input = rFind rprogram

printfn $"input={input}"
