type PlayerState(p1Pos:int64,score1:int64,p2Pos:int64,score2:int64) =
    member this.Player1 = p1Pos
    member this.Player2 = p2Pos
    member this.Score1 = score1
    member this.Score2 = score2
    member this.Leader = if score1 > score2 then 1 else 2 
    override this.ToString () = $"PlayerState(player1@{p1Pos}={score1},player2@{p2Pos}={score2})"
    member this.IsWin = score1 > 20L || score2 > 20L 

let prodPlayers = PlayerState (9L,0L,6L,0L)
let testPlayers = PlayerState (4L,0L,8L,0L)

type Board(first:int64,last:int64) =
    member this.Min = first
    member this.Max = last
    member this.StepsFrom (pos:int64) (steps:int64) =
        let pos = pos - 1L
        let pos = pos + steps
        let pos = pos % (last)
        pos + 1L

let board = Board(1L,10L)
    
let distRolls =
    let die12 = Seq.allPairs {1..3} {1..3} |> Seq.map (fun (d1,d2) -> d1+d2)
    let die123 = Seq.allPairs die12 {1..3} |> Seq.map (fun (d12,d3) -> d12+d3)
    let dieDist =
        die123
        |> Seq.toList 
        |> List.groupBy id
        |> List.map (fun (roll,rolls) -> (roll,rolls.Length |> int64))
    dieDist


type State(players:PlayerState,pathCount:int64,turn:int64) =
    member this.Players = players
    member this.Paths = pathCount
    member this.Turn = turn
    override this.ToString () =
        $"State({players} paths={pathCount} turn={turn}"
    member this.MergeKey = [|players.Player1;players.Player2;players.Score1;players.Score2;turn|]

type States(states:list<State>,winsFor1:int64,winsFor2:int64) =
    member this.States = states
    member this.WinsFor1 = winsFor1
    member this.WinsFor2 = winsFor2 
    override this.ToString () = $"States([wins1:{winsFor1} wins2:{winsFor2} #={states.Length} {states})"
    
    member this.Merge () =
        let grouped = states |> List.groupBy (fun s -> s.MergeKey)
        let grouped =
            grouped
            |> List.map (fun (_,states) -> states.Head,states)
            |> List.map (fun (state,states) -> (state,states |> List.map (fun s -> s.Paths)))
            |> List.map (fun (state,paths) -> (state, paths |> List.sum))
            |> List.map (fun (state,paths) -> State(state.Players,paths,state.Turn))
        printfn $"merge: {states.Length} -> {grouped.Length}"
        States(grouped,winsFor1,winsFor2)
        
    member this.ExtractWinners () =
        let isWinner1 (state:State) = state.Players.Leader = 1
        let isWinner2 (state:State) = state.Players.Leader = 2
        let winners : list<State> = states |> List.filter (fun (state) -> state.Players.IsWin)
        let winners1 : int64 = winners
                               |> List.filter isWinner1
                               |> List.map (fun state -> state.Paths)
                               |> List.sum
        let winners2 : int64 = winners
                               |> List.filter isWinner2
                               |> List.map (fun state -> state.Paths)
                               |> List.sum
        printfn $"## winners1={winners1} winners2={winners2}"

        let winsFor1 = winsFor1 + winners1
        let winsFor2 = winsFor2 + winners2
        
        let undecided = states |> List.filter (fun (state) -> state.Players.IsWin |> not)
        printfn $"winners: {winners} undecided = {undecided}"
        States(undecided,winsFor1,winsFor2)
    
let playRound (state:State) =
    if state.Turn = 1 then 
        let pos = state.Players.Player1
        let score = state.Players.Score1
        distRolls |> List.map (fun (roll,paths) ->
            let pos = board.StepsFrom pos roll
            let score = score + pos 
            let paths = state.Paths * paths
            let players = PlayerState(pos,score,state.Players.Player2,state.Players.Score2)
            State(players,paths,2))
    else
        let pos = state.Players.Player2
        let score = state.Players.Score2
        distRolls |> List.map (fun (roll,paths) ->
            let pos = board.StepsFrom pos roll
            let score = score + pos 
            let paths = state.Paths * paths
            let players = PlayerState(state.Players.Player1,state.Players.Score1,pos,score)
            State(players,paths,1))

let playRounds (states:States) : States =
    let newStates = states.States |> List.map playRound |> List.concat 
    printfn $"newStates = {newStates}"
    let states = States(newStates,states.WinsFor1,states.WinsFor2)
    let states = states.Merge ()
    let states : States = states.ExtractWinners ()
    states
    
let initPlayers = prodPlayers 

let initState = State(initPlayers,1L,1)
// let rolls1 = playRound initState 

let initStates: States = States([initState],0L,0L)

let states1 = playRounds initStates
let states2 = playRounds states1

let states3 = playRounds states2 
let states4 = playRounds states3 
let states5 = playRounds states4 
let states6 = playRounds states5 
let states7 = playRounds states6 

printfn $"states(7) = {states7}"

// printfn $"{rolls1}"

printfn "##### Playing out"
    
let rec playOut (states:States) =
    if states.States.Length > 0 then
        let states = playRounds states
        playOut states
    else
        printfn $"endState: {states}"
        
playOut initStates

492043106122795
267086464416104