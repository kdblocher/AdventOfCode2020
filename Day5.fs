module Day5

let row2bit =
  function
  | 'F' -> 0
  | 'B' -> 1
  | _ -> failwith "Invalid row character"
let col2bit =
  function
  | 'L' -> 0
  | 'R' -> 1
  | _ -> failwith "Invalid column character"

let partition f = Seq.map f >> Seq.fold (fun s -> (+) (s <<< 1)) 0
let parseCol = partition col2bit
let parseRow = partition row2bit

type Seat = {
  Row: int
  Col: int
}
let seatId s = s.Row * 8 + s.Col

open System.Text.RegularExpressions
let parseLine input =
  let regex = Regex.Match (input, "([FB]+)([LR]+)")
  let row = regex.Groups.[1].Value
  let col = regex.Groups.[2].Value
  { Row = parseRow row
    Col = parseCol col }

let part1 : Seat seq -> int =
  Seq.map seatId >> Seq.max

let part2 : Seat seq -> int =
  Seq.sortBy seatId
  >> Seq.filter (fun s -> s.Row > 0)
  >> Seq.map seatId
  >> Seq.pairwise
  >> Seq.find (fun (a, b) -> a + 2 = b)
  >> fst
  >> (+) 1

open System
open System.IO
let run () =
  "input/Day5.txt"
  |> File.ReadAllLines
  |> Seq.map parseLine
  |> part2
  |> Console.WriteLine

[<EntryPoint>]
let main _ =
  run ()
  0