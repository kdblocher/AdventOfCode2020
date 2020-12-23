module Day12

type Direction =
  | North
  | East
  | South
  | West
let getVector value =
  function
  | North -> -value, 0
  | East -> 0, +value
  | South -> +value, 0
  | West -> 0, -value

let directions = [| North; East; South; West |]

type Rotation =
  | Left
  | Right

open FSharpPlus.Math.Generic
let rotate rotation angle direction =
  let binOp = match rotation with Left -> (-) | Right -> (+)
  let index = remE (binOp (Array.findIndex ((=) direction) directions) (angle / 90)) directions.Length
  index |> Array.get directions

type Transposition =
  | Forward

open FSharpPlus

let combine (x1, y1) (x2, y2) =
  (x1 + x2), (y1 + y2)

type Position = {
  Location: int * int
  Direction: Direction
}
let move pos value direction =
  { pos with Location = combine pos.Location (getVector value direction) }

type Action =
  | Cardinal of Direction
  | Move of Transposition
  | Rotate of Rotation

let actOnce pos value =
  function
  | Cardinal d -> move pos value d
  | Move m -> move pos value pos.Direction
  | Rotate r -> { pos with Direction = rotate r value pos.Direction }

let act =
  Seq.fold (fun pos (action, value) -> actOnce pos value action) { Location = 0, 0; Direction = East }

let part1 =
  act >> (fun { Location = (a, b) } -> a + b )

let parseAction =
  function
  | 'N' -> Cardinal North
  | 'E' -> Cardinal East
  | 'S' -> Cardinal South
  | 'W' -> Cardinal West
  | 'L' -> Rotate Left
  | 'R' -> Rotate Right
  | 'F' -> Move Forward
  | _ -> failwith "Invalid input"

open System.Text.RegularExpressions
let parseLine input =
  let regex = Regex.Match (input, "^(\w)(\d+)$")
  let action = regex.Groups.[1].Value.[0] |> parseAction
  let value = regex.Groups.[2].Value |> int
  action, value
  
open System
open System.IO
let run () =
  "input/Day12.txt"
  |> File.ReadAllLines
  |> Seq.map parseLine
  |> part1
  |> Console.WriteLine
