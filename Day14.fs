module Day14

type Mask = {
  And: uint64
  Or:  uint64
}

type Mem = {
  Location: uint64
  Value: uint64
}

type Instruction =
  | Mask of Mask
  | Mem of Mem

type State = {
  Mask: Mask
  Memory: (uint64, uint64) Map
}

let executeOnce state =
  function
  | Mask mask ->
    { state with Mask = mask }
  | Mem mem ->
    { state with Memory = state.Memory |> Map.add mem.Location (mem.Value &&& state.Mask.And ||| state.Mask.Or) }

open System
let execute : _ seq -> State =
  { Mask = { And = UInt64.MaxValue; Or = UInt64.MinValue }
    Memory = Map.empty
  }
  |> Seq.fold executeOnce

let part1 : _ seq -> _ =
  execute
  >> fun state -> state.Memory
  >> Map.toSeq
  >> Seq.map snd
  >> Seq.sum

type MaskBit = Unchanged | Zero | One
let makeAndMaskBit = function Zero -> 0UL | _ -> 1UL
let makeOrMaskBit = function One -> 1UL | _ -> 0UL
let makeMask f = Seq.fold (fun s b -> (s <<< 1) + f b)
let makeAndMask = makeMask makeAndMaskBit (UInt64.MaxValue &&& (268435456UL <<< 36))
let makeOrMask = makeMask makeOrMaskBit UInt64.MinValue

let parseMaskBit =
  function
  | 'X' -> Unchanged
  | '1' -> One
  | '0' -> Zero
  | _ -> failwith "Unexpected input"

open System.Text.RegularExpressions
let parseMask input =
  let regex = Regex.Match (input, "^mask = ([X01]+)$")
  let mask = regex.Groups.[1].Value
  let parsed = mask |> Seq.map parseMaskBit |> Seq.cache
  { And = parsed |> makeAndMask
    Or = parsed |> makeOrMask
  }

let parseMem input =
  let regex = Regex.Match (input, "^mem\[(\d+)\] = (\d+)$")
  let loc = regex.Groups.[1].Value |> uint64
  let value = regex.Groups.[2].Value |> uint64
  { Location = loc
    Value = value }

let parse (input: string) = 
  if input.StartsWith "mask"
  then Mask <| parseMask input
  else Mem <| parseMem input

open System
open System.IO
let run () =
  "input/Day14.txt"
  |> File.ReadAllLines
  |> Seq.map parse
  |> part1
  |> Console.WriteLine