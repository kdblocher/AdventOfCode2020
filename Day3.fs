module Day3

open FSharpPlus

type Square =
  | Open
  | Tree

let parse = function
  | '.' -> Open
  | '#' -> Tree
  | _ -> failwith "parse error"

let encounter pos (line: _ array) =
  line.[pos % line.Length] = Tree

let checkSlope right down =
  Seq.mapi (fun i x -> i, x)
  >> Seq.where (fun (i, x) -> i % down = 0 && encounter ((i / down) * right) x)
  >> Seq.length

let part1 lines =
  lines |> checkSlope 3 1

open System.IO
open System
let part2 lines =
  let lines = lines |> Seq.cache
  [1, 1; 3, 1; 5, 1; 7, 1; 1, 2]
  |> Seq.map (fun (a, b) -> checkSlope a b lines)
  |> Seq.fold (*) 1

open System
open System.IO
open Microsoft.FSharp.Core.Operators

let run =
  "input/Day3.txt"
  |> File.ReadAllLines :> _ seq
  |> Seq.map (Seq.map parse >> Seq.toArray)
  |> part2
  |> Console.WriteLine