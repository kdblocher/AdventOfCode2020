module Day1

open FSharpPlus

let getTwoEntriesSumToTarget target (data: _ seq) =
  query {
    for e1 in data do
    for e2 in data do
    where (e1 + e2 = target)
    select (e1, e2)
  }

let part1 =
  Seq.head
  >> (fun (a, b) -> a * b)

open System
open System.IO
open Microsoft.FSharp.Core.Operators

let run =
  "input/Day1.txt"
  |> File.ReadAllLines :> _ seq
  |> Seq.map int
  |> getTwoEntriesSumToTarget 2020
  |> part1
  |> Console.Write

[<EntryPoint>]
let main _ =
  run
  0