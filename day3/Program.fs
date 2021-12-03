

open System
open System.IO

let readFile (fileName: String) = seq {
   use sr = new StreamReader(fileName)
   while not sr.EndOfStream do
      yield sr.ReadLine ()
}

let lines = readFile("input.txt") |> Seq.toArray

printfn $"File: {lines}"