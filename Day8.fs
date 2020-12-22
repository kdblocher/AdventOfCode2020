module Day8

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

type ProgramResult = {
  Accumulator: int
  Counter: int
}

module Seq =
  open FSharpPlus
  let foldWhile folder state =
    Some state
    |> Seq.scan (fun state e -> 
      state >>= (fun state -> folder state e))
    >> Seq.takeWhile Option.isSome
    >> Seq.last
    >> Option.get

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
  { Accumulator = 0; Counter = 0 }
  |> Seq.unfold (fun state ->
    let lineResult = executeOnce lines.[state.Counter] state.Accumulator
    let state' = { Accumulator = lineResult.Accumulator; Counter = lineResult.Offset + state.Counter }
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
  |> part1
  |> Console.WriteLine
