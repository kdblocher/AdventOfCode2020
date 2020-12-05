module Day2

open FSharpPlus
open System.Text.RegularExpressions

type Policy = {
  Lower: int
  Upper: int
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
      Lower = min
      Upper = max
      Char = letter }
    Password = password }

let part1valid policy password =
  let count = Regex.Matches(password, policy.Char |> string).Count
  count >= policy.Lower && count <= policy.Upper

let part2valid policy (password: string) =
  let check pos = password.[pos - 1] = policy.Char
  (check policy.Lower) <> (check policy.Upper)

let countValid validator =
  Seq.where (fun line -> validator line.Policy line.Password)
  >> Seq.length

open System
open System.IO
open Microsoft.FSharp.Core.Operators

let run =
  "input/Day2.txt"
  |> File.ReadAllLines :> _ seq
  |> Seq.map parseLine
  |> countValid part2valid
  |> Console.WriteLine