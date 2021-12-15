module aoc2020_day23.Ring

open aoc2020_day23.Types
open aoc2020_day23.Segment 


let rec private take n (segments:Segment list) =
    match segments with
    | [] -> None
    | segment::tail ->
        if segment.Length >= n then
            let cups,segment = segment.Take n
            Some(cups,(segment::tail))
        else
            let cups1,_ = segment.TakeAll ()
            match take (n - segment.Length) tail with
            | None -> None
            | Some(cups,segments) -> Some(List.concat cups1 cups,segments) 

type Ring(start:Segment list,rtail:Segment list) =
    member this.Start = start
    member this.RTail = rtail
    member this.takeThe3 (): (Cup*Cup*Cup)*Ring =
        match take 4 start with
        | None -> (this.gc ()).takeThe3 ()
        | Some ([cup1;cup2;cup3;cup4],[]) ->
            (cup2,cup3,cup4),Ring([Segment.single cup1],rtail)
        | Some ([cup1;cup2;cup3;cup4],segment::segments) ->
            (cup2,cup3,cup4),Ring(segment::segments,rtail)
    member this.gc () : Ring = this
    
    static member fromDigits (cups:Cup list) =
        Ring([Segment(cups.Length,Mess cups)],[])
          