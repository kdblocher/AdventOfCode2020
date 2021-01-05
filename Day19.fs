module Day19
open FSharpx.Collections

type Id = int
type Rule =
  | Character of char
  | Sequence of Rule LazyList
  | Disjunction of Rule LazyList

type Rules = (Id, Rule) Map

type CheckResult =
  | Success
  | More of Rule

let rec check char rule =
  seq {
    match rule with
    | Character c ->
      if char = c then yield Success
    | Sequence none when none.IsEmpty ->
      yield Success
    | Sequence rule when rule.Tail.IsEmpty ->
      yield! check char rule.Head
    | Sequence rules ->
      yield! check char rules.Head
      |> Seq.map (
        function
        | Success -> More (Sequence rules.Tail)
        | More r -> More (Sequence (rules.Tail |> LazyList.cons r)))
    | Disjunction rules ->
      yield! rules |> Seq.collect (check char)
  }

let isMatch rule =
  let rec loop results =
    function
    | [] ->
      results |> Seq.contains Success
    | char :: rest ->
      results |> Seq.exists (
        function
        | More rule -> loop (check char rule) rest
        | _ -> false)
  loop (Seq.singleton (More rule))

module Parse =
  open FSharpPlus

  type Definition =
    | Rule of Rule
    | RefSequence of int list
    | DefDisjunction of Definition * Definition

  let rec walk env =
    let find r = Map.find r env
    function
    | Rule x ->
      x
    | DefDisjunction (r1, r2) ->
      Disjunction (seq { walk env r1; walk env r2 } |> LazyList.ofSeq)
    | RefSequence [ref] ->
      find ref |> walk env
    | RefSequence refs ->
      refs
      |> Seq.map (find >> walk env)
      |> LazyList.ofSeq
      |> Sequence

  let parseSequence =
    String.split (Seq.singleton " ")
    >> Seq.map int
    >> Seq.toList
    >> RefSequence

  open System.Text.RegularExpressions
  let parseRuleLine input = 
    let regex = Regex.Match (input, "^(\d+)\: (\"([a-z])\"|(((\d+ ?)+)( \| ((\d+ ?)+))*))$")
    let id = regex.Groups.[1].Value |> int
    let char = regex.Groups.[3].Value
    let first = regex.Groups.[5].Value
    let second = regex.Groups.[8].Value
    id,
    let (isInt, _) = System.Int32.TryParse char
    if not isInt && char.Length > 0 then Rule (Character char.[0])
    elif second = "" then parseSequence first
    else DefDisjunction (parseSequence first, parseSequence second)

  let parseRules : _ seq -> _ =
    Seq.map parseRuleLine
    >> Map.ofSeq
    >> fun m -> m |> Map.find 0 |> walk m

  let parseMessage = Seq.toList

  let parse input =
    let sets = Regex.Split (input, "\r\n\r\n")
    Regex.Split (sets.[0], "\r\n") |> parseRules,
    Regex.Split (sets.[1], "\r\n") |> Seq.map parseMessage

let solve (rule, messages) =
  messages
  |> Seq.filter (isMatch rule)
  |> Seq.length

open System
open System.IO
let run () =
  File.ReadAllText "input/Day19.txt"
  |> Parse.parse
  |> solve
  |> Console.WriteLine