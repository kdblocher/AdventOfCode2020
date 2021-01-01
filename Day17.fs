module Day17
open System

let init =
  Seq.mapi (fun x ->
    Seq.mapi (fun y ->
      Seq.mapi (fun z c ->
        c, [x; y; z])))
  >> Seq.collect id
  >> Seq.collect id
  >> Seq.choose (fun (cube, pos) -> if cube then Some pos else None)
  >> set

let update count active =
  (active && (count = 2 || count = 3))
  || (not active && count = 3) 

let cycleOnce active =
  let bound i = 
    active
    |> Set.toSeq
    |> Seq.map (List.skip i >> List.head)
    |> (fun d -> Seq.min d, Seq.max d) 
  let x0, x' = bound 0
  let y0, y' = bound 1
  let z0, z' = bound 2

  seq { x0 - 1 .. x' - x0 + 2 } |> Seq.collect (fun x ->
    seq { y0 - 1 .. y' - z0 + 2 } |> Seq.collect (fun y ->
      seq { z0 - 1 .. z' - z0 + 2 } |> Seq.map (fun z ->
        let pos = [x; y; z]
        let count =
          seq { -1 .. 1 } |> Seq.collect (fun dx ->
            seq { -1 .. 1 } |> Seq.collect (fun dy ->
              seq { -1 .. 1 } |> Seq.map (fun dz ->
                [dx; dy; dz])))
          |> Seq.filter ((<>) [0; 0; 0])
          |> Seq.map (List.map2 (+) pos)
          |> Seq.filter (fun d -> active |> Set.contains d)
          |> Seq.length
        pos, count)))
  |> Seq.fold (fun active' (pos, count) ->
    if active |> Set.contains pos |> update count
    then Set.add pos active'
    else active')
    Set.empty

let cycle count =
  let rec loop i active =
    if i = count then active else loop (i + 1) (cycleOnce active)
  loop 0

let part1 =
  init
  >> cycle 6
  >> Set.count

let parseChar =
  function
  | '.' -> false
  | '#' -> true
  | _ -> failwith "Invalid cube character"

let parseLine : _ seq -> _ =
  Seq.map (parseChar >> Seq.singleton)

let parse : _ seq -> _ =
  Seq.map parseLine
 
open System.IO
let run () =
  "input/Day17.txt"
  |> File.ReadAllLines
  |> parse
  |> part1
  |> Console.WriteLine