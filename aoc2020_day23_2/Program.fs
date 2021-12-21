open aoc2020_day23_2.SimpleRing
open aoc2020_day23_2.ComplexRing
open aoc2020_day23_2.Types

// type Ring = SimpleRing
type Ring = ComplexRing

let stringToRing (s: string) =
    s.ToCharArray()
    |> Array.toList
    |> List.map (fun c -> (int c) - (int '0'))
    |> Ring.build

let nextDown (ring: Ring) (cup: Cup) = if cup = 1 then ring.Max else cup - 1

let rec shrinkTillFree (ring: Ring) (cup: Cup) (taken: list<Cup>) =
    if taken |> List.contains cup then
        let cup = nextDown ring cup
        shrinkTillFree ring cup taken
    else
        cup

let crabTurn (ring: Ring) : Ring =
    let takenCups, ring = ring.TakeThe3Cups()
    let destinationCup = nextDown ring (ring.Head())

    let destinationCup =
        shrinkTillFree ring destinationCup takenCups

    let ring : Ring  =
        ring.insertAfter destinationCup takenCups

    ring.Step()



let testRing: Ring = "389125467" |> stringToRing
let prodRing: Ring = "398254716" |> stringToRing

let ring100 (ring: Ring) =
    { 0 .. 99 }
    |> Seq.fold (fun ring _ -> crabTurn ring) ring


printfn $"test ring(100) {ring100 testRing}"
printfn $"prod ring(100) {ring100 prodRing}"
