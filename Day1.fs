module Day1

open FSharpPlus

let getTwoEntriesSumToTarget target (data: _ seq) =
  query {
    for e1 in data do
    for e2 in data do
    where (e1 + e2 = target)
    select (e1, e2)
  }

let getThreeEntriesSumToTarget target (data: _ seq) =
  query {
    for e1 in data do
    for e2 in data do
    for e3 in data do
    where (e1 + e2 + e3 = target)
    select (e1, e2, e3)
  }

let part1 : (int * int) seq -> _ =
  Seq.head
  >> (fun (a, b) -> a * b)

let part2 : (int * int * int) seq -> _ =
  Seq.head
  >> (fun (a, b, c) -> a * b * c)

open System
open System.IO
open Microsoft.FSharp.Core.Operators

let run =
  "input/Day1.txt"
  |> File.ReadAllLines :> _ seq
  |> Seq.map int
  |> getThreeEntriesSumToTarget 2020
  |> part2
  |> Console.Write