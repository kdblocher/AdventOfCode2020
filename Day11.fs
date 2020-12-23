module Day11

type Location =
| Floor
| Empty
| Occupied
let occupied = function Occupied -> true | _ -> false

let genOnce adjacent tolerance area =
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
        if adjacent area x y |> Seq.filter occupied |> Seq.length >= tolerance
        then changed <- true; Empty
        else Occupied
    ))
  |> fun area -> if changed then Some area else None

let gen genOnce area =
  let rec loop area =
    match genOnce area with
    | Some area -> loop area
    | None -> area
  loop area

let solve adjacent tolerance =
  gen (genOnce adjacent tolerance)
  >> Seq.collect id
  >> Seq.filter occupied
  >> Seq.length

let immediateAdjacent (area: _ array array) x y =
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

let part1 =
  solve immediateAdjacent 4

let octoAdjacent (area: _ array array) x y =
  let rec look x y dx dy =
    let x = x + dx
    let y = y + dy
    if x < 0 || y < 0 || x >= area.Length || y >= area.[x].Length
    then None
    else
      match area.[x].[y] with
      | Floor -> look x y dx dy
      | x -> Some x
  seq {
    look x y -1 -1
    look x y -1 +0
    look x y -1 +1
    look x y +0 -1
    look x y +0 +1
    look x y +1 -1
    look x y +1 +0
    look x y +1 +1
  }
  |> Seq.choose id

let part2 =
  solve octoAdjacent 5

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
  |> part2
  |> Console.WriteLine