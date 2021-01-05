module Day19

type Id = int
type Rule =
  | Character of char
  | Sequence of Rule list
  | Disjunction of Rule * Rule

type Rules = (Id, Rule) Map

type CheckResult =
  | Succ
  | Fail
  | More of Rule

let rec check pos char =
  function
  | Character c ->
    if char = c then Succ else Fail
  | Disjunction (a, b) ->
    let f = check pos char
    match f a, f b with
    | Fail, Fail        -> Fail
    | Fail, r | r, Fail -> r
    | Succ, _ | _, Succ -> Succ
    | More a, More b ->
      More (Disjunction (a, b))
  | Sequence [] ->
    Succ
  | Sequence [rule] ->
    check pos char rule
  | Sequence (rule :: rest) ->
    match check pos char rule with
    | Fail -> Fail
    | Succ -> More (Sequence rest)
    | More r -> More (Sequence (r :: rest))

let isMatch rule =
  let rec loop pos rule =
    function
    | [] -> rule = Succ
    | char :: rest ->
      match rule with
      | More rule ->
        let result = check pos char rule
        loop (pos + 1) result rest
      | _ -> false
  loop 0 (More rule)

module Parse =
  open FSharpPlus

  type Def =
    | Rule of Rule
    | RefSequence of int list
    | DefDisjunction of Def * Def

  let rec walk env =
    let find r = Map.find r env
    function
    | Rule x ->
      x
    | DefDisjunction (r1, r2) ->
      Disjunction (walk env r1, walk env r2)
    | RefSequence [ref] ->
      find ref |> walk env
    | RefSequence refs ->
      refs
      |> List.map (find >> walk env)
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

let part1 (rule, messages) =
  messages
  |> Seq.filter (isMatch rule)
  |> Seq.length

open System
open System.IO
let run () =
  File.ReadAllText "input/Day19.txt"
  |> Parse.parse
  |> part1
  |> Console.WriteLine