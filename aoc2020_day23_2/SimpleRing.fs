module aoc2020_day23_2.SimpleRing

open aoc2020_day23_2.Types

type SimpleRing(cups: list<Cup>) =
    let highestCup:Cup = cups |> List.max 
    member this.Cups = cups
    member this.Max = cups |> List.max 
    
    member this.TakeThe3Cups () =
        match cups with
        | a::b::c::d::rest -> [b;c;d],SimpleRing(a::rest)
        | _ -> failwith $"not enough cups in cuplist: {cups}"
        
    member this.insertAfter (dest:Cup) (taken:list<Cup>) =
        let rec insert (cups:list<Cup>) =
            if cups.Head = dest then
                cups.Head :: List.concat [taken;cups.Tail] 
            else
                cups.Head :: insert cups.Tail
        let cups = insert cups
        SimpleRing(cups) 
    member this.Head () = cups.Head
    
    member this.Step () =
        SimpleRing (List.concat [cups.Tail;[cups.Head]])
    
    override this.ToString () =
        let cups = cups |> List.map string |> String.concat ""
        $"【{cups}】"