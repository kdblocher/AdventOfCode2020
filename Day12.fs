module Day12

type Direction =
  | North
  | East
  | South
  | West

let directions = [| North; East; South; West |]

let getVector value =
  function
  | North -> 0, +value
  | East -> +value, 0
  | South -> 0, -value
  | West -> -value, 0

type Rotation =
  | Left
  | Right

type Transposition =
  | Forward

module Tuple =
  let inline map op (x1, y1) (x2, y2) =
    op x1 x2, op y1 y2

type Action =
  | Cardinal of Direction
  | Move of Transposition
  | Rotate of Rotation

module Part1 =

  type Position = {
    Location: int * int
    Direction: Direction
  }

  let moveShip pos value direction =
    { pos with Location = Tuple.map (+) pos.Location (getVector value direction) }

  open FSharpPlus.Math.Generic
  let rotateShip rotation angle direction =
    let binOp = match rotation with Left -> (-) | Right -> (+)
    let index = remE (binOp (Array.findIndex ((=) direction) directions) (angle / 90)) directions.Length
    index |> Array.get directions

  let actOnce pos value =
    function
    | Cardinal d -> moveShip pos value d
    | Move m -> moveShip pos value pos.Direction
    | Rotate r -> { pos with Direction = rotateShip r value pos.Direction }

  let act : _ seq -> Position =
    Seq.fold (fun pos (action, value) -> actOnce pos value action) { Location = 0, 0; Direction = East }

let part1 : _ seq -> int =
  Part1.act >> (fun { Location = (a, b) } -> abs a + abs b )

module Part2 =

  type Position = {
    Location: int * int
    Waypoint: int * int
  }

  let moveWaypoint pos value direction =
    { pos with Waypoint = Tuple.map (+) pos.Waypoint (getVector value direction) }
  let moveShip pos value = 
    let vectorWaypoint = Tuple.map (*) (value, value) pos.Waypoint
    { pos with Location = Tuple.map (+) pos.Location vectorWaypoint }

  open FSharpPlus.Math.Generic
  let rotateWaypoint rotation angle (x, y) =
    let unOp = match rotation with Left -> (-) 0 | Right -> (+) 0
    let index = remE (unOp (angle / 90)) directions.Length
    match index with
    | 0 -> +x, +y
    | 1 -> +y, -x
    | 2 -> -x, -y
    | 3 -> -y, +x
    | _ -> failwith "Unexpected rotation"

  let actOnce pos value =
    function
    | Cardinal d -> moveWaypoint pos value d
    | Move m -> moveShip pos value
    | Rotate r -> { pos with Waypoint = rotateWaypoint r value pos.Waypoint }

  let act : _ seq -> Position =
    Seq.fold (fun pos (action, value) -> actOnce pos value action) { Location = 0, 0; Waypoint = 10, 1 }

let part2 : _ seq -> int =
  Part2.act >> (fun { Location = (a, b) } -> abs a + abs b )

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
  |> part2
  |> Console.WriteLine
