module Day14

module List =
  let cons a b = a :: b

type MaskBit = Floating | Zero | One
type Mem = {
  Location: uint64
  Value: uint64
}

type Instruction =
  | Mask of MaskBit seq
  | Mem of Mem

type State = {
  Mask: MaskBit seq
  Memory: (uint64, uint64) Map
}

type Mask = {
  And: uint64
  Or:  uint64
}

open System
let bits = 36
let zeroOr = UInt64.MinValue
let zeroAnd = UInt64.MaxValue &&& (268435456UL <<< bits) // 2^(64-36)
let makeAndMaskBit = function Zero -> 0UL | _ -> 1UL
let makeOrMaskBit = function One -> 1UL | _ -> 0UL
let makeMaskBits f = Seq.fold (fun s b -> (s <<< 1) + f b)
let makeAndMask = makeMaskBits makeAndMaskBit zeroAnd
let makeOrMask = makeMaskBits makeOrMaskBit zeroOr
let makeMask bits = 
  let bits = bits |> Seq.cache
  { And = bits |> makeAndMask
    Or = bits |> makeOrMask
  }
let applyMask1 mask value = value &&& mask.And ||| mask.Or

let execute1 state =
  function
  | Mask mask ->
    { state with Mask = mask }
  | Mem mem ->
    let mask = makeMask state.Mask
    { state with Memory = state.Memory |> Map.add mem.Location (applyMask1 mask mem.Value) }

let initialState = {
  Mask = Seq.init bits (fun _ -> Floating)
  Memory = Map.empty
}

let solve f =
  initialState
  |> Seq.fold f
  >> fun state -> state.Memory
  >> Map.toSeq
  >> Seq.sumBy snd

let part1 : _ seq -> _ =
  solve execute1

let applyMask2 loc mask =
  let rec loop mask shift : MaskBit list list =
    match mask with
    | [] -> [[]]
    | bit :: mask ->
      loop mask (shift - 1)
      |> match bit with      
          | Floating ->
            List.collect (fun ls -> [ Zero :: ls ; One :: ls ])
          | One ->
            List.map (List.cons One)
          | Zero ->
            List.map (List.cons (if ((loc >>> shift) &&& 1UL) = 1UL then One else Zero))
  loop mask (bits - 1)
  |> Seq.toList
  |> Seq.map makeOrMask
  |> Seq.toList

let execute2 state =
  function
  | Mask mask ->
    { state with Mask = mask }
  | Mem mem ->
    let memory =
      state.Mask
      |> Seq.toList
      |> applyMask2 mem.Location
      |> Seq.fold (fun m mask -> m |> Map.add mask mem.Value) state.Memory
    { state with Memory = memory }

let part2 : _ seq -> _ =
  solve execute2

let parseMaskBit =
  function
  | 'X' -> Floating
  | '1' -> One
  | '0' -> Zero
  | _ -> failwith "Unexpected input"

open System.Text.RegularExpressions
let parseMask input =
  let regex = Regex.Match (input, "^mask = ([X01]+)$")
  let mask = regex.Groups.[1].Value
  mask |> Seq.map parseMaskBit

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
  |> part2
  |> Console.WriteLine