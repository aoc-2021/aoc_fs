module aoc2020_day23_2.ComplexRing

open aoc2020_day23_2.Types

type Chaos(cups: list<Cup>) =
    let cupSet = cups |> Set
    member this.Size = cups.Length

    member this.ContainsCup(cup: Cup) = cupSet.Contains cup

    member this.Take4() =
        if cups.Length < 4 then
            failwith $"Tried to take 4 cups from Chaos of size {this.Size}"

        cups |> List.take 4, Chaos(cups |> List.skip 4)

    member this.Head = cups.Head
    member this.Tail = cups.Tail

    member this.InsertAfter (dest: Cup) (taken: list<Cup>) =
        printfn $"InsertAfter {dest} {taken} cups={cups}"

        let rec insert (cups: list<Cup>) =
            match cups with
            | first :: rest when first = dest -> List.concat [ first :: taken; rest ]
            | first :: rest -> first :: insert rest

        let cups = insert cups
        Chaos(cups)

    member this.AddToFront(cup: Cup) = Chaos(cup :: cups)

    member this.Step() =
        let cups = List.append cups.Tail [ cups.Head ]
        Chaos(cups)

    override this.ToString() =
        let cups =
            cups |> List.map string |> String.concat ","

        $"⦕{cups}⦖"


type Increasing(first: Cup, last: Cup) =
    member this.Size = last - first + 1

    member this.ContainsCup(cup: Cup) = cup >= first && cup <= last

    member this.Take4 =
        if this.Size < 5 then
            failwith $"Tried to take 4 cups from Increasing [{first}->{last}]"

        let cups = [ 0; 1; 2; 3 ] |> List.map ((+) first)
        cups, Increasing(first + 4, last)

    member this.Head = first

type Segment =
    | LeafC of Chaos
    | LeafI of Increasing
    | Composite of int * List<Segment>

type ComplexRing(cups: Segment, max: Cup) =
    member this.Cups = cups
    member this.Max = max

    member this.TakeThe3Cups() : list<Cup> * ComplexRing =
        match cups with
        | LeafC chaos ->
            let rings, chaos = chaos.Take4()
            rings.Tail, ComplexRing(LeafC(chaos.AddToFront rings.Head), max)
        | _ -> failwith $"Not implemented {cups}"

    member this.insertAfter (dest: Cup) (taken: list<Cup>) : ComplexRing =
        match cups with
        | LeafC chaos -> ComplexRing(LeafC(chaos.InsertAfter dest taken), max)
        | _ -> failwith $"Not implemented [insertAfter] {cups}"

    member this.Head() =
        match cups with
        | LeafC chaos -> chaos.Head
        | _ -> failwith $"Not implemented [Head] {cups}"

    member this.Step() : ComplexRing =
        match cups with
        | LeafC chaos -> ComplexRing(LeafC(chaos.Step()), max)
        | _ -> failwith $"Not implemented [Step]{cups}"

    override this.ToString() = $"【{cups.ToString()}】"

    static member build(cups: list<Cup>) =
        let chaos = Chaos cups
        let segment = LeafC chaos
        ComplexRing(segment, 9)
