module Day6

type Question = char
type QuestionGroup = Question seq seq

let sumBy f =
  Seq.sumBy (Seq.map Set >> Seq.reduce f >> Seq.length)

let part1 : QuestionGroup seq -> _ = sumBy Set.union
let part2 : QuestionGroup seq -> _ = sumBy Set.intersect

open System.Text.RegularExpressions
let parseLine input : Question seq =
  let regex = Regex.Match (input, "([a-z]+)")
  let row = regex.Groups.[1].Value
  row |> seq

let parseGroup input : QuestionGroup =
  Regex.Split (input, "\r\n")
  |> Seq.map parseLine
  
let parseFile input =
  Regex.Split (input, "\r\n\r\n")
  |> Seq.map parseGroup

open System
open System.IO
let run () =
  "input/Day6.txt"
  |> File.ReadAllText
  |> parseFile
  |> part2
  |> Console.WriteLine