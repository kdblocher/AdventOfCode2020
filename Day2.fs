module Day2

open FSharpPlus
open System.Text.RegularExpressions

type Policy = {
  Min: int
  Max: int
  Char: char
}
type InputLine = {
  Policy: Policy
  Password: string
}

let parseLine input =
  let regex = Regex.Match (input, "(\d+)\-(\d+) (.)\: (.+)")
  let min = regex.Groups.[1].Value |> int
  let max = regex.Groups.[2].Value |> int
  let letter = regex.Groups.[3].Value |> char
  let password = regex.Groups.[4].Value
  { Policy = {
      Min = min
      Max = max
      Char = letter }
    Password = password }

let isValid policy password =
  let count = Regex.Matches(password, policy.Char |> string).Count
  let result = count >= policy.Min && count <= policy.Max
  result

let part1 =
  Seq.where (fun line -> isValid line.Policy line.Password)
  >> Seq.length

let part2 : (int * int * int) seq -> _ =
  Seq.head
  >> (fun (a, b, c) -> a * b * c)

open System
open System.IO
open Microsoft.FSharp.Core.Operators

let run =
  "input/Day2.txt"
  |> File.ReadAllLines :> _ seq
  |> Seq.map parseLine
  |> part1
  |> Console.WriteLine

[<EntryPoint>]
let main _ =
  run
  0