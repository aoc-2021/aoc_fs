open System.IO

let file =
    File.ReadAllLines "input.txt" |> Array.toList

type Reg =
    | W
    | X
    | Y
    | Z

type Inst =
    | INP of Reg
    | ADDI of Reg*int64
    | ADDR of Reg*Reg
    | MULI of Reg*int64
    | MULR of Reg*Reg
    | DIVI of Reg*int64
    | DIVR of Reg*Reg
    | MODI of Reg*int64
    | MODR of Reg*Reg
    | EQLI of Reg*int64
    | EQLR of Reg*Reg

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

printfn $"program = {program}"
// ok, lets try reverse again

