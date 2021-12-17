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
    

// let (minInitY,maxInitY) = velosY testTarget steps

// printfn $"velos: {(minInitY,maxInitY)}"

// let initVelo : Velocity = (initX,maxInitY)

let stopped ((minX,_,_,_):Area) ((x,_):Position) ((dx,_):Velocity) =
    dx = 0 && x < minX 

let rec simulate (target:Area) (pos:Position) (velocity:Velocity) : bool*Position =
    if isHit target pos then true,pos
    else if stopped target pos velocity then false,pos 
    else if isMiss target pos then false,pos
    else
        let (pos,velocity) = step pos velocity
        simulate target pos velocity

let underShootsX ((minX,_,_,_)) ((x,_):Position) = x < minX

let overShootsX ((_,_,maxX,_)) ((x,_):Position) = x > maxX
 
let belowY ((_,minY,_,_)) ((_,y):Position) = y < minY
let aboveY ((_,_,_,maxY)) ((_,y):Position) = y > maxY

let rec findHighestY (target:Area) ((veloX,veloY):Velocity) : Velocity =
    let result,_ = simulate target startPos (veloX,veloY+1L)
    if result then
        findHighestY target (veloX,veloY+1L)
    else
        (veloX,veloY)
    
let rec find (target:Area) ((veloX,veloY):Velocity) (soFar:list<Velocity>): bool*Velocity*List<Velocity> =
  if overShootsX target (veloX,veloY) then false,(veloX,veloY),soFar
  else 
    let result,pos = simulate target startPos (veloX,veloY)
    printfn $"{veloX},{veloY} -> {result},{pos}"
    if result then
        let (_,maxVeloY) = findHighestY target (veloX,veloY)
        let veloRange = {veloY .. maxVeloY} |> Seq.map (fun y -> (veloX,y)) |> Seq.toList 
        printfn $"Adding velos: {veloRange}"
        let soFar = List.concat [soFar;veloRange] 
        find target (veloX+1L,-10L) soFar 
        // true,maxVelo,soFar
    else
        if belowY target pos then
            find target (veloX,veloY+1L) soFar
        else if underShootsX target pos then
            find target (veloX+1L,-10L) soFar
        else false,(veloX,veloY),soFar 
            
// let _,_,velocities = find testTarget (0L,0L) []
// printfn $"findResult: ${velocities}"

let rec maxHeight (curr:int64) (dx,dy) : int64 =
    if dy <= 0L then curr
    else maxHeight (curr + dy) (dx,dy-1L)  

let best (velos:List<Velocity>) =
    let velos = velos |> List.filter (fun velo -> (snd velo > 0L))
    let velos = (0L,0L)::velos 
    printfn $"positives: {velos}"
    let posHeights = velos |> List.map (maxHeight 0L)
    printfn $"posHeights: {posHeights}"
    posHeights |> List.max 

// let bestHeight = best velocities 

let solve (target:Area) =
    let _,_,velos = find target (0L,0L) []
    let height = best velos
    height
    
let solution = solve testTarget

printfn $"task 1: {solution}"

// let x = simulate testTarget startPos (6L,3L)

let validX ((x1,_,x2,_):Area) (dx:int64) =
    printfn $"validX {dx}"
    let rec step x dx =
       if x > x2 then
           printfn $"overshot: ${x}"
           false
       elif dx < 1L && x < x1 then
           printfn $"stopped: ${x}"
           false
       elif x <= x2 && x >= x1 then
           printfn $"valid: {x}"
           true
       else
           printfn $"step {x+dx} {dx-1L}"
           step (x+dx) (dx - 1L)
    step 0L dx        
       
// printfn $"{x}"

let rec findYs (target:Area) ((dx,dy):Velocity): list<Velocity> =
    let (x1,y1,x2,y2) = target
    printfn $"findYs: {target} ({dx},{dy})"
    let nextPos = (dx,dy+1L)
    let result,pos = simulate target startPos (dx,dy)
    if dy > (-y1) then [] 
    elif result then
        printfn $"success: {dx},{dy}"
        (dx,dy)::findYs target nextPos
    elif belowY target pos then
        if belowY target (fst pos,- (snd pos)) then []
        else findYs target nextPos 
    else [] 

let solve2 (target:Area) =
    let (x1,y1,x2,y2) = target 
    let dxes = {0L..x2+1L} |> Seq.filter (validX target) |> Seq.toList
    let velos = dxes |> List.map (fun dx -> findYs target (dx,0L)) |> List.concat
    printfn $"{velos |> List.length}"
    let best = best velos
    printfn $"solve2: {best}"

solve2 target

let underX1 ((x1,_,x2,_):Area) ((x,y):Position) ((dx,dy):Velocity)  = dx = 0 && x < x1 
    
let rec sim2 (target:Area) (pos:Position) (velo:Velocity) =
    if pos = startPos then printfn $"sim2 {velo}"
    if isHit target pos then true  
    elif belowY target pos then
        false
    else if overShootsX target pos then
        false
    else if underX1 target pos velo then 
        false
    else
        let pos,velo = step pos velo
        sim2 target pos velo 

let solve3 ((x1,y1,x2,y2):Area) =
    let velos = Seq.allPairs {0L..(x2+1L)} {(y1-1L)..(-y1+1L)} |> Seq.toList |> List.sort 
    printfn $"solve3:velos={velos}"
    let results = velos
                  |> List.map (sim2 (x1,y1,x2,y2) startPos)
                  |> List.filter id
    let results2 = results |> List.length
    printfn $"task 2: {results2} {results}"
    
solve3 target 