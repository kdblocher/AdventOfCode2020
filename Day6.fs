module Day6

type Question = char
type QuestionGroup = {
  Questions: Question seq 
  People: Question seq seq
}

let part1 : QuestionGroup seq -> int =
  Seq.sumBy (fun g -> g.Questions |> Seq.length)

open System.Text.RegularExpressions
let parseLine input : Question seq =
  let regex = Regex.Match (input, "([a-z]+)")
  let row = regex.Groups.[1].Value
  row |> seq

let parseGroup input : QuestionGroup =
  let questions =
    Regex.Split (input, "\r\n")
    |> Seq.map parseLine
  { People = questions
    Questions = questions |> Seq.collect id |> Seq.distinct
  }
  
let parseFile input =
  Regex.Split (input, "\r\n\r\n")
  |> Seq.map parseGroup

open System
open System.IO
let run () =
  "input/Day6.txt"
  |> File.ReadAllText
  |> parseFile
  |> part1
  |> Console.WriteLine