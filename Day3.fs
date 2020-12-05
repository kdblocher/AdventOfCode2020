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
  Seq.mapi (fun i -> encounter (i * right))
  >> Seq.mapi (fun i x -> i, x)
  >> Seq.where (fun (i, x) -> x && i % down = 0)
  >> Seq.length

open System
open System.IO
open Microsoft.FSharp.Core.Operators

let run =
  "input/Day3.txt"
  |> File.ReadAllLines :> _ seq
  |> Seq.map (Seq.map parse >> Seq.toArray)
  |> checkSlope 3 1
  |> Console.WriteLine

[<EntryPoint>]
let main _ =
  run
  0