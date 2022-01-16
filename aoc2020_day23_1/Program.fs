// let input = "398254716" // actual
let input = "389125467" // test 

type Ring (next:Map<int,int>, prev:Map<int,int>,start:int) =
    member this.Current = start
    
    static member ofString(s:string, extended: bool) =
        let values = s.ToCharArray () |> Array.map (fun i -> (int i) - (int '0')) 
        printfn $"values={values |> Array.toList }"
        let froms = [0..values.Length-2]
        printfn $"vals = {values}"
        let next = froms |> List.map (fun i -> values[i],values[i+1]) |> Map 
        let prev = froms |> List.map (fun i -> values[i+1],values[i]) |> Map
        printfn $"next={next}"
        printfn $"prev={prev}"
        let start = values[0]
        let last = values[values.Length-1]
        printfn $"start={start} last={last}" 
        let next = next.Add (last,start)
        let prev = prev.Add (start,last)
        if extended then
            let last = prev[start]
            let nextExtra = [10..999_999] |> List.map (fun i -> i,i+1)
            let prevExtra = [10..999_999] |> List.map (fun i -> i+1,i)
            let next = List.concat [(next |> Map.toList);nextExtra] |> Map
            let prev = List.concat [(prev |> Map.toList);prevExtra] |> Map  
            let next = next.Add(last,10).Add(1_000_000,start)
            let prev = prev.Add(10,last).Add(start,1_000_000)
            Ring (next,prev,start)
        else 
            Ring(next,prev,start)
    
    member this.Take3 () :int*int*int*Ring =
        let first = next[start]
        let second = next[first]
        let third = next[second]
        let fourth = next[third]
        let next = next.Add(start,fourth)
        let prev = prev.Add(fourth,start)
        let ring = Ring(next,prev,start)
        first,second,third,ring 
    
    member this.PlaceAfter (target:int,first:int,second:int,third:int) =
        let after = next[target]
        let next = next.Add(target,first).Add(first,second).Add(second,third).Add(third,after)
        let prev = prev.Add(first,target).Add(second,first).Add(third,second).Add(after,third)
        Ring(next,prev,start)
    
    member this.Step () =
        Ring(next,prev,next[start])
        
    member this.SetStart (start:int) =
        Ring(next,prev,start)
        
    override this.ToString () =
        let rec getn (n:int) (item:int) : list<int> =
            if n = 0 then []
            else item ::  (getn (n-1) next[item])
        let first10 = getn 9 start
        let s = first10 |> List.map string |> String.concat " "
        $"Ring({s})"
   

let crabTurn (ring:Ring) : Ring =
    let first,second,third,ring = ring.Take3 ()
    // printfn $"cups = {first}{second}{third} ring={ring}"
    let rec nextTarget (i:int) =
        let i = if i < 1 then 9 else i 
        if (i = first || i = second || i = third) then nextTarget (i-1) else i
    let target = nextTarget (ring.Current - 1)
    let ring = ring.PlaceAfter (target,first,second,third)
    let ring = ring.Step()
    // printfn $"ring={ring}"
    
     
    ring 

let ring = Ring.ofString (input,true)
printfn $"ring: {ring}"

let rec crabTurns (n:int) (ring:Ring) : Ring =
   if (n % 100000 = 0) then printfn $"{n}"
   if n = 0 then ring
   else crabTurns (n-1) (crabTurn ring)

let ring2 = crabTurns 10_000_000 ring |> (fun ring -> ring.SetStart 1)
printfn $"ring2 = {ring2}"