module Day20

open System.Text.RegularExpressions

type Color = Black | White
type Tile = {
  Id: int
  Body: Color seq seq
}

type Side = Top | Right | Bottom | Left
type Orientation = Normal | Flipped
type Id = int
type Hash = int16
type Border = {
  Id: Id
  Side: Side
  Orientation: Orientation
  Hash: Hash
}

let toBit =
  function Black -> 1s | White -> 0s

let compliment = 2 
let getBorderHash =
  Seq.fold (fun s -> toBit >> (+) (s <<< 1)) 0s

let getBorders { Id = id; Body = body } =
  let makeBorder o s f = { Id = id; Side = s; Orientation = o; Hash = body |> f |> getBorderHash }
  seq {
    makeBorder Normal Top Seq.head
    makeBorder Normal Left (Seq.map Seq.head)
    makeBorder Normal Bottom Seq.last
    makeBorder Normal Right (Seq.map Seq.last)
    makeBorder Flipped Top (Seq.head >> Seq.rev)
    makeBorder Flipped Left (Seq.map Seq.head >> Seq.rev)
    makeBorder Flipped Bottom (Seq.last >> Seq.rev)
    makeBorder Flipped Right (Seq.map Seq.last >> Seq.rev)
  }

open FSharpPlus

let groupTiles tiles =
  let allBorders = tiles |> Seq.collect getBorders |> Seq.toArray
  tiles
  |> Seq.groupBy (fun t ->
    getBorders t
    |> Seq.filter (fun b -> allBorders |> Seq.exists (fun b2 -> b.Id <> b2.Id && b.Hash = b2.Hash))
    |> Seq.length)
  |> Seq.toList

let part1 : _ seq -> _ =
  groupTiles
  >> Seq.find (fst >> (=) 4)
  >> snd
  >> Seq.map (fun b -> b.Id |> int64)
  >> Seq.fold (*) 1L
  
module Parse =
  let parsePixel =
    function
    | '.' -> White
    | '#' -> Black
    | _ -> failwith "Invalid tile"

  let parseBody =
    Seq.map (Seq.map parsePixel >> Seq.cache)
    >> Seq.cache

  let parseId input =
    let regex = Regex.Match (input, "Tile (\d+):")
    regex.Groups.[1].Value |> int

  let parseTile input =
    { Id = input |> Seq.head |> parseId
      Body = input |> Seq.tail |> parseBody }

  let parse input =
    Regex.Split(input, "\r\n\r\n")
    |> Seq.map ((fun input -> Regex.Split (input, "\r\n")) >> parseTile)

open System
open System.IO
let run () =
  File.ReadAllText "input/Day20.txt"
  |> Parse.parse
  |> part1
  |> Console.WriteLine