module Day10

let part1 =
  Seq.append (Seq.singleton 0) // charging outlet
  >> Seq.sort
  >> Seq.toArray
  >> Seq.pairwise
  >> Seq.map (fun (a, b) -> b - a)
  >> Seq.groupBy id
  >> Seq.map (fun (a, b) -> a, Seq.length b)
  >> Map
  >> (fun m ->
    Map.find 1 m
    * (Map.find 3 m
      + 1)) // built-in adapter

let parse =
  Array.map int

open System
open System.IO
let run () =
  "input/Day10.txt"
  |> File.ReadAllLines
  |> parse
  |> part1
  |> Console.WriteLine