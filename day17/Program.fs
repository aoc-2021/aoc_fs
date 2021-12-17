open System.IO
open System.Text.RegularExpressions

let file = File.ReadAllLines "input.txt"

type Position = int64 * int64

type Velocity(dx:int64, dy:int64) =
    member this.Dx = dx
    member this.Dy = dy
    
    member this.FirstPos : Position = (dx,dy)
    override this.ToString () =
        $"Velocity({dx},{dy})"

type Area = int64 * int64 * int64 * int64

let parseInput (input:string[]) : Option<Area> = 
    match Regex.Match (file.[0],@"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)*") with
    | failed when not failed.Success -> None
    | result ->
       let [x1;x2;y1;y2] = result.Groups.Values |> Seq.toList |> List.tail |> List.map string |> List.map int64
       Some (x1,y1,x2,y2)



let target : Area = parseInput file |> Option.get 

// let target = fileArea file

let testTarget = (20L, -10L, 30L, -5L)
let startPos: Position = 0, 0

let step ((posX, posY): Position) (velocity: Velocity) : Position * Velocity =
    let posX = posX + velocity.Dx
    let posY = posY + velocity.Dy

    let xAdjust =
        if velocity.Dx = 0L then 0L
        else if velocity.Dx > 0L then -1L
        else 1L

    let veloX = velocity.Dx + xAdjust
    let veloY = velocity.Dy - 1L
    (posX, posY), Velocity(veloX, veloY)

let isHit ((minX, minY, maxX, maxY): Area) ((x, y): Position) =
    x >= minX && x <= maxX && y >= minY && y <= maxY

let isMiss ((minX, minY, maxX, maxY): Area) ((x, y): Position) = x > maxX || y < minY

let maxStepsX ((minX, _, maxX, _): Area) : int64 * int64 =
    let buffer = maxX - minX

    let rec step (targetX: int64) (targetSpeed: int64) : int64 * int64 =
        if targetX < buffer && targetX - targetSpeed < 0 then
            0, targetSpeed + 1L
        else
            let steps, initSpeed =
                step (targetX - targetSpeed) (targetSpeed + 1L)

            (steps + 1L), initSpeed

    step maxX 0L

let velosY ((_, minY, _, maxY): Area) (steps: int64) =
    let rec gravityLoss (steps: int64) =
        if steps = 1 then
            0L
        else
            let prevLoss = gravityLoss (steps - 1L)
            prevLoss * 2L + 1L

    let loss = gravityLoss steps
    let targetMinY = minY + loss
    let targetMaxY = maxY + loss
    let minVelo = targetMinY / steps
    let maxVelo = targetMaxY / steps
    (minVelo, maxVelo)

let stopped ((minX, _, _, _): Area) ((x, _): Position) (velocity: Velocity) = velocity.Dx = 0 && x < minX

let rec simulate (target: Area) (pos: Position) (velocity: Velocity) : bool * Position =
    if isHit target pos then
        true, pos
    else if stopped target pos velocity then
        false, pos
    else if isMiss target pos then
        false, pos
    else
        let (pos, velocity) = step pos velocity
        simulate target pos velocity

let underShootsX ((minX, _, _, _)) ((x, _): Position) = x < minX

let overShootsX ((_, _, maxX, _)) ((x, _): Position) = x > maxX

let belowY ((_, minY, _, _)) ((_, y): Position) = y < minY
let aboveY ((_, _, _, maxY)) ((_, y): Position) = y > maxY

let rec findHighestY (target: Area) (velocity: Velocity) : Velocity =
    let result, _ = simulate target startPos (Velocity(velocity.Dx, velocity.Dy + 1L))

    if result then
        findHighestY target (Velocity(velocity.Dx, velocity.Dy + 1L))
    else
        velocity

let rec find (target: Area) (velocity: Velocity) (soFar: list<Velocity>) : bool * Velocity * List<Velocity> =
    if overShootsX target velocity.FirstPos then
        false, velocity, soFar
    else
        let result, pos = simulate target startPos velocity

        if result then
            let maxVelocity = findHighestY target velocity

            let veloRange =
                { velocity.Dy .. maxVelocity.Dy }
                |> Seq.map (fun y -> Velocity(velocity.Dx, y))
                |> Seq.toList

            let soFar = List.concat [ soFar; veloRange ]
            find target (Velocity(velocity.Dx + 1L, -10L)) soFar
        else if belowY target pos then
            find target (Velocity(velocity.Dx, velocity.Dy + 1L)) soFar
        else if underShootsX target pos then
            find target (Velocity(velocity.Dx + 1L, -10L)) soFar
        else
            false, velocity, soFar

let rec maxHeight (curr: int64) (velocity:Velocity) : int64 =
    if velocity.Dy <= 0L then
        curr
    else
        maxHeight (curr + velocity.Dy) (Velocity(velocity.Dx, velocity.Dy - 1L))

let best (velos: List<Velocity>) =
    let velos =
        velos |> List.filter (fun velo -> (velo.Dy > 0L))

    let velos = Velocity (0L, 0L) :: velos
    let posHeights = velos |> List.map (maxHeight 0L)
    posHeights |> List.max

let solve (target: Area) =
    let _, _, velos = find target (Velocity(0L, 0L)) []
    let height = best velos
    height

let solution = solve testTarget

printfn $"task 1: {solution}"

let validX ((x1, _, x2, _): Area) (dx: int64) =
    let rec step x dx =
        if x > x2 then false
        elif dx < 1L && x < x1 then false
        elif x <= x2 && x >= x1 then true
        else step (x + dx) (dx - 1L)

    step 0L dx

let rec findYs (target: Area) (velocity: Velocity) : list<Velocity> =
    let _, y1, _, _ = target
    let nextPos = Velocity(velocity.Dx, velocity.Dy + 1L)
    let result, pos = simulate target startPos  velocity

    if velocity.Dy > (-y1) then
        []
    elif result then
        velocity :: findYs target nextPos
    elif belowY target pos then
        if belowY target (fst pos, -(snd pos)) then
            []
        else
            findYs target nextPos
    else
        []

let solve2 (target: Area) =
    let _, _, x2, _ = target

    let dxes =
        { 0L .. x2 + 1L }
        |> Seq.filter (validX target)
        |> Seq.toList

    let velos =
        dxes
        |> List.map (fun dx -> findYs target (Velocity(dx, 0L)))
        |> List.concat

    let best = best velos
    printfn $"task 1: {best}"

solve2 target

let underX1 ((x1, _, x2, _): Area) ((x, y): Position) (velocity: Velocity) = velocity.Dx = 0 && x < x1

let rec sim2 (target: Area) (pos: Position) (velo: Velocity) =
    if isHit target pos then
        true
    elif belowY target pos then
        false
    else if overShootsX target pos then
        false
    else if underX1 target pos velo then
        false
    else
        let pos, velo = step pos velo
        sim2 target pos velo

let solve3 ((x1, y1, x2, y2): Area) =
    let velos =
        Seq.allPairs { 0L .. (x2 + 1L) } { (y1 - 1L) .. (-y1 + 1L) }
        |> Seq.map Velocity 
        |> Seq.toList
        |> List.sortBy (fun v -> (v.Dx,v.Dy))

    let results =
        velos
        |> List.map (sim2 (x1, y1, x2, y2) startPos)
        |> List.filter id
        |> List.length

    printfn $"task 2: {results}"

solve3 target
