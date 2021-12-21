open System.IO

let file = File.ReadAllLines "input.txt"

type PlayerState(p1Pos:int64,p1Score:int64,p2Pos:int64,p2Score:int64) =
    member this.Player1 = p1Pos
    member this.Player2 = p2Pos
    member this.Score1 = p1Score
    member this.Score2 = p2Score
    override this.ToString () = $"PlayerState(player1@{p1Pos}={p1Score},player2@{p2Pos}={p2Score})"

let prodPlayers = PlayerState (9L,0L,6L,0L)
let testPlayers = PlayerState (4L,0L,8L,0L)
type Board(min:int64,max:int64) =
    member this.Min = min
    member this.Max = max
    
    member this.StepsFrom (pos:int64) (steps:int64) =
        let pos = pos - 1L 
        let pos = pos + steps
        let pos = pos % (max)
        pos + 1L 
     

let board = Board(1L,10L)

    
printfn $"7+5 -> {board.StepsFrom 7 5}"        

let dieRoll (dieNum:int64) (rollNum:int64) =
    let zeroBased = (((rollNum - 1L)* 3L) + (dieNum-1L)) % 100L
    let oneBased = zeroBased + 1L
    oneBased

// 0-based
let dieRollsSum (round:int64): int64 =
    let dieRound = round + 1L
    printfn $"dieRound = {dieRound}"
    let die1 = dieRoll 1 dieRound 
    let die2 = dieRoll 2 dieRound
    let die3 = dieRoll 3 dieRound
    printfn $"roll(round={round} {die1} {die2} {die3} = {die1+die2+die3}"
    die1 + die2 + die3

// 0-based
let playRound (board:Board) (players:PlayerState) (round:int64) =
    if (round % 2L) = 0L then
        // player 1
        let steps = dieRollsSum round
        let p1pos = board.StepsFrom players.Player1 steps
        let p1score = players.Score1 + p1pos
        printfn $"(1) steps = {steps} players={players}"
        PlayerState(p1pos,p1score,players.Player2,players.Score2)
    else
        // player 2
        let steps = dieRollsSum round
        let p2pos = board.StepsFrom players.Player2 steps
        let p2score = players.Score2 + p2pos
        printfn $"(2) steps = {steps} players={players}"
        printfn $"p2pos={p2pos}"
        PlayerState(players.Player1,players.Score1,p2pos,p2score)
        
let r0res = playRound board testPlayers 0L
let r1res = playRound board r0res 1L
printfn $"r0res = {r0res}"
printfn $"r1res = {r1res}"

printfn $"board:move 10 + 1 {board.StepsFrom 10L 1L}"

let past1000 (players:PlayerState) = players.Score1 > 999 || players.Score2 > 999

let playTo1000 (board:Board) (initPlayers:PlayerState) : int64*PlayerState =
    let rec playto1000 (players:PlayerState) (round:int64) =
        if past1000 players
        then round,players
        else
            let players = playRound board players round
            playto1000 players (round+1L)
    playto1000 initPlayers 0L 

let (rounds,at1000) = playTo1000 board prodPlayers
let dieRolls = rounds * 3L

let winnerScore (players:PlayerState) =
    if (players.Score1 > 999L) then players.Score1 else players.Score2
let loserScore (players:PlayerState) =
    if (players.Score1 > 999L) then players.Score2 else players.Score1


let winScore = winnerScore at1000
let loseScore = loserScore at1000 

printfn $"@1000 = rolls:{dieRolls},{loseScore} {dieRolls*loseScore}"