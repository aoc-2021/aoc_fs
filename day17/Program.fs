open System.IO

let file = File.ReadAllLines "input.txt"

type Velocity = int64*int64
type Position = int64*int64

type Area = int64*int64*int64*int64

let fileArea (file:string[]) : Area =
    let parts : string[] = file.[0].Split '='
    let parts2 = parts.[1].Split ',' 
    let xes = parts2.[0].Split '.'
    let yes = parts.[2].Split '.'
    printfn $"{yes |> Array.toList }" 
    let minX = xes.[0] |> int64
    let maxX = xes.[2] |> int64
    let minY = yes.[0] |> int64
    let maxY = yes.[2] |> int64
    minX,minY,maxX,maxY
    
let target = fileArea file

let testTarget = (20L,-10L,30L,-5L)
let startPos: Position = 0,0 

printfn $"target: {target}"


let step ((posX,posY):Position) ((veloX,veloY):Velocity) : Position*Velocity =
    let posX = posX + veloX
    let posY = posY + veloY
    let xAdjust = if veloX = 0L then 0L else if veloX > 0L then -1L else 1L
    let veloX = veloX + xAdjust
    let veloY = veloY - 1L
    (posX,posY),(veloX,veloY)
    
let isHit ((minX,minY,maxX,maxY):Area) ((x,y):Position) = x >= minX && x <= maxX && y >= minY && y <= maxY
let isMiss ((minX,minY,maxX,maxY):Area) ((x,y):Position) = x > maxX || y < minY

let maxStepsX ((minX,_,maxX,_):Area) : int64*int64 =
    let buffer = maxX - minX
    let rec step (targetX:int64) (targetSpeed:int64) : int64*int64 =
        if targetX < buffer && targetX - targetSpeed < 0 then
            0,targetSpeed+1L 
        else 
            let steps,initSpeed = step (targetX - targetSpeed) (targetSpeed+1L)
            (steps + 1L),initSpeed
    step maxX 0L

let velosY ((_,minY,_,maxY):Area) (steps:int64) =
    let rec gravityLoss (steps:int64) =
        if steps = 1 then 0L
        else 
            let prevLoss = gravityLoss (steps - 1L)
            prevLoss * 2L + 1L
    let loss = gravityLoss steps
    let targetMinY = minY+loss
    let targetMaxY = maxY+loss
    let minVelo = targetMinY / steps
    let maxVelo = targetMaxY / steps
    printfn $"velos: {minVelo} {maxVelo}"
    (minVelo,maxVelo)
    
let (steps,initX) = maxStepsX testTarget 

printfn $"steps: {steps}"        

let (minInitY,maxInitY) = velosY testTarget steps

printfn $"velos: {(minInitY,maxInitY)}"

let initVelo : Velocity = (initX,maxInitY)

let stopped ((minX,_,_,_):Area) ((x,_):Position) ((dx,_):Velocity) =
    dx = 0 && x < minX 

let rec simulate (target:Area) (pos:Position) (velocity:Velocity) : bool*Position =
    if isHit target pos then true,pos
    else if stopped target pos velocity then false,pos 
    else if isMiss target pos then false,pos
    else
        let (pos,velocity) = step pos velocity
        simulate target pos velocity

let res = simulate testTarget startPos initVelo

printfn $"res = {res}"

let underShootsX ((minX,_,_,_)) ((x,_):Position) = x < minX

let overShootsX ((_,_,maxX,_)) ((x,_):Position) = x < maxX
 
let belowY ((_,minY,_,_)) ((_,y):Position) = y < minY
let aboveY ((_,_,_,maxY)) ((_,y):Position) = y > maxY

let rec findHighestY (target:Area) ((veloX,veloY):Velocity) : Velocity =
    let result,_ = simulate target startPos (veloX,veloY+1L)
    if result then
        findHighestY target (veloX,veloY+1L)
    else
        (veloX,veloY)
    
let rec find (target:Area) ((veloX,veloY):Velocity) : bool*Velocity =
    let result,pos = simulate target startPos (veloX,veloY)
    printfn $"{veloX},{veloY} -> {result},{pos}"
    if result then
        let maxVelo = findHighestY target (veloX,veloY)
        true,maxVelo
    else
        if belowY target pos then
            find target (veloX,veloY+1L)
        else if underShootsX target pos then
            find target (veloX+1L,-10L)
        else false,(veloX,veloY)
            
let findResult = find testTarget (0L,0L)
printfn $"findResult: ${findResult}"

let x = simulate testTarget startPos (6L,3L)

printfn $"{x}"
