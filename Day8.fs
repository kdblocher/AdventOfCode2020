module Day8

module Seq =
  open FSharpPlus
  let foldWhile folder state =
    Some state
    |> Seq.scan (fun state e -> 
      state >>= (fun state -> folder state e))
    >> Seq.takeWhile Option.isSome
    >> Seq.last
    >> Option.get

type Instruction =
  | NoOperation
  | Accumulate
  | Jump

type Line = {
  Instruction: Instruction
  Operand: int
}

type LineResult = {
  Accumulator: int
  Offset: int
}

type ProgramState = {
  Accumulator: int
  Counter: int
  Terminated: bool
}

let parseInstruction =
  function
  | "nop" -> NoOperation
  | "acc" -> Accumulate
  | "jmp" -> Jump
  | _ -> failwith "Invalid instruction"

let executeOnce { Instruction = instruction; Operand = operand } acc =
  match instruction with
  | NoOperation -> { Accumulator = acc; Offset = +1 }
  | Accumulate -> { Accumulator = acc + operand; Offset = +1 }
  | Jump -> { Accumulator = acc; Offset = operand }

let execute (lines: _ array) =
  { Accumulator = 0; Counter = 0; Terminated = false }
  |> Seq.unfold (fun state ->
    if state.Terminated then Some (state, state)
    else
      let lineResult = executeOnce lines.[state.Counter] state.Accumulator
      let counter = lineResult.Offset + state.Counter
      let state' = { Accumulator = lineResult.Accumulator; Counter = counter; Terminated = counter = lines.Length }
      Some (state, state'))

let executeUntil lines =
  execute lines
  |> Seq.foldWhile (fun (_, visited) nextResult ->
    if visited |> Set.contains nextResult.Counter
    then None
    else Some (Some nextResult, (visited |> Set.add nextResult.Counter))) (None, Set.empty)
  |> fst
  |> Option.get

let part1 =
  executeUntil >> (fun result -> result.Accumulator)

let swapInstruction = function
  | Jump -> NoOperation
  | NoOperation -> Jump
  | x -> x

let gen lines =
  lines
  |> Seq.mapi (fun i line ->
    let lines' = lines |> Array.copy
    lines'.[i] <- { line with Instruction = swapInstruction line.Instruction }
    lines')

let part2 =
  gen
  >> Seq.map executeUntil
  >> Seq.find (fun s -> s.Terminated)
  >> fun s -> s.Accumulator

open System.Text.RegularExpressions
let parseLine input =
  let regex = Regex.Match (input, "(\w{3}) ([+-]\d+)")
  let instruction = regex.Groups.[1].Value |> parseInstruction
  let operand = regex.Groups.[2].Value |> int
  { Instruction = instruction
    Operand = operand
  }

open System
open System.IO
let run () =
  "input/Day8.txt"
  |> File.ReadAllLines
  |> Array.map parseLine
  |> part2
  |> Console.WriteLine
