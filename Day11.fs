module Day11

type Location =
  | Floor
  | Empty
  | Occupied
let occupied = function Occupied -> true | _ -> false

let adjacent (area: _ array array) x y =
  seq {
    if (x > 0) then
      if y > 0 then yield area.[x - 1].[y - 1]
      yield area.[x - 1].[y]
      if y < area.[x - 1].Length - 1 then yield area.[x - 1].[y + 1]
    if y > 0 then yield area.[x].[y - 1]
    if y < area.[x].Length - 1 then yield area.[x].[y + 1]
    if (x < area.Length - 1) then
      if y > 0 then yield area.[x + 1].[y - 1]
      yield area.[x + 1].[y]
      if y < area.[x + 1].Length - 1 then yield area.[x + 1].[y + 1]
  }

let genOnce area =
  let mutable changed = false
  area
  |> Array.mapi (fun x ->
    Array.mapi (fun y ->
      function
      | Floor -> Floor
      | Empty ->
        if adjacent area x y |> Seq.exists occupied |> not
        then changed <- true; Occupied
        else Empty
      | Occupied ->
        if adjacent area x y |> Seq.filter occupied |> Seq.length >= 4
        then changed <- true; Empty
        else Occupied
    ))
  |> fun area -> if changed then Some area else None

open FSharpPlus
let rec gen area =
  match genOnce area with
  | Some area -> gen area
  | None -> area

let part1 =
  gen
  >> Seq.collect id
  >> Seq.filter occupied
  >> Seq.length

let parseChar =
  function
  | '.' -> Floor
  | 'L' -> Empty
  | '#' -> Occupied
  | _ -> failwith "invalid input"

let parseLine =
  Seq.map parseChar
  >> Seq.toArray

let parse =
  Seq.map parseLine
  >> Seq.toArray

open System
open System.IO
let run () =
  "input/Day11.txt"
  |> File.ReadAllLines
  |> parse
  |> part1
  |> Console.WriteLine