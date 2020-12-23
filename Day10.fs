module Day10

let part1 : int seq -> int =
  Seq.pairwise
  >> Seq.map (fun (a, b) -> b - a)
  >> Seq.groupBy id
  >> Seq.map (fun (a, b) -> a, Seq.length b)
  >> Map
  >> (fun m -> Map.find 1 m * Map.find 3 m)
  
let part2 input =
  input
  |> Seq.skip 1
  |> Seq.fold (fun m a ->
    m |> Map.add a (
      seq { 1 .. 3 }
      |> Seq.choose (fun b -> Map.tryFind (a - b) m)
      |> Seq.sum))
    (Map.empty |> Map.add 0 1L)
  |> Map.find (input |> Array.last)

let prep =
  Array.sort
  >> (fun input ->
  [|
    Array.singleton 0
    input
    input |> Array.last |> (+) 3 |> Array.singleton
  |] |> Array.concat)

let parse =
  Array.map int

open System
open System.IO
let run () =
  "input/Day10.txt"
  |> File.ReadAllLines
  |> parse
  |> prep
  |> part2
  |> Console.WriteLine