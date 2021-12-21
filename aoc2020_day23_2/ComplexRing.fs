module aoc2020_day23_2.ComplexRing

open aoc2020_day23_2.Types

type Chaos(cups:list<Cup>) =
    let cupSet = cups |> Set 
    member this.Size = cups.Length
   
    member this.ContainsCup (cup:Cup)  = cupSet.Contains cup
    member this.Take4 () =
        if cups.Length < 4 then
            failwith $"Tried to take 4 cups from Chaos of size {this.Size}"
        cups |> List.take 4,Chaos(cups |> List.skip 4)
    member this.Head = cups.Head
    member this.Tail = cups.Tail
    member this.InsertAfter (dest:Cup) (taken:list<Cup>) =
        let rec insert (cups:list<Cup>) =
            match cups with
            | first::rest when first = dest ->
                List.concat [first::taken;rest]
            | first::rest -> first::insert rest
        let cups = insert cups
        Chaos(cups)
    member this.AddToFront (cup:Cup) = Chaos(cup::cups)
    member this.Step () =
        let cups = List.append cups.Tail [cups.Head]
        Chaos(cups)
    
    override this.ToString () =
        let cups = cups |> List.map string |> String.concat ","
        $"⦕{cups}⦖"
    

type Increasing(first:Cup,last:Cup) =
    member this.Size = last - first + 1
    
    member this.ContainsCup (cup:Cup) = cup >= first && cup <= last
    member this.Take4 =
        if this.Size < 5 then failwith $"Tried to take 4 cups from Increasing [{first}->{last}]"
        let cups = [0;1;2;3] |> List.map ((+) first)
        cups,Increasing(first+4,last)
    member this.Head = first

type Segment =
    | LeafC of Chaos
    | LeafI of Increasing
    | Composite of int * List<Segment> 

type ComplexRing(cups: Segment) =
    let highestCup:Cup = 10_000_000
    member this.Cups = cups
    member this.Max = highestCup 
    
    member this.TakeThe3Cups () : list<Cup>*ComplexRing =
        match cups with
        | LeafC chaos ->
            let rings,chaos = chaos.Take4 ()
            rings.Tail, ComplexRing(LeafC (chaos.AddToFront rings.Head)) 
        | _ -> failwith $"Not implemented {cups}"
        
    member this.insertAfter (dest:Cup) (taken:list<Cup>) =
        match cups with
        | LeafC chaos -> chaos.InsertAfter dest taken 
        | _ -> failwith $"Not implemented [insertAfter] {cups}"  
    member this.Head () =
        match cups with
        | LeafC chaos -> chaos.Head
        | _ -> failwith $"Not implemented [Head] {cups}"
    
    member this.Step () =
        match cups with
        | LeafC chaos -> chaos.Step ()
        | _ -> failwith $"Not implemented [Step]{cups}"
    
    override this.ToString () =
        $"【{cups.ToString ()}】"
    
    static member build (cups:list<Cup>) =
        let chaos = Chaos cups
        let segment = LeafC chaos 
        ComplexRing(segment)