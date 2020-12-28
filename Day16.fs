module Day16

type Range = {
  Lower: int
  Upper: int
}

type Field = {
  Name: string
  Ranges: Range array
}

type Notes = {
  Fields: Field array
  MyTicket: int array
  NearbyTickets: int array array
}

let combine (r1, r2) =
  if   r1.Lower < r2.Upper && r1.Upper >= r2.Lower + 1 then Some { Lower = r1.Lower; Upper = r2.Upper }
  elif r2.Lower < r1.Upper && r2.Upper >= r1.Lower + 1 then Some { Lower = r2.Lower; Upper = r1.Upper }
  else None

let consolidate =
  let rec scan =
    function
    | [] -> []
    | [r] -> [r]
    | r1 :: r2 :: ranges ->
      match combine (r1, r2) with
      | Some r -> r :: scan ranges
      | None -> r1 :: scan (r2 :: ranges)
  Array.sortBy (fun r -> r.Lower, r.Upper)
  >> List.ofArray
  >> scan
  >> List.toArray

let inRange v r =
  v >= r.Lower && v <= r.Upper

let validate { Fields = fields } =
  let ranges =
    fields
    |> Array.collect (fun f -> f.Ranges)
    |> Array.sortBy (fun r -> r.Lower, r.Upper)
    |> consolidate
  fun v -> ranges |> Seq.exists (inRange v)

let part1 notes =
  let validate = validate notes
  notes.NearbyTickets
  |> Seq.collect id
  |> Seq.filter (not << validate)
  |> Seq.sum

open System.Text.RegularExpressions

let parseTicket input =
  Regex.Split (input, ",")
  |> Array.map int

let parseField input =
  let regex = Regex.Match (input, "^(.+): (\d+)-(\d+) or (\d+)-(\d+)$")
  let name = regex.Groups.[1].Value
  let lower1 = regex.Groups.[2].Value |> int
  let upper1 = regex.Groups.[3].Value |> int
  let lower2 = regex.Groups.[4].Value |> int
  let upper2 = regex.Groups.[5].Value |> int
  { Name = name
    Ranges = [|
      { Lower = lower1; Upper = upper1 }
      { Lower = lower2; Upper = upper2 }
    |]
  }

let parse input =
  let sections = Regex.Split (input, "\r\n\r\n")
  let splitLines input = Regex.Split (input, "\r\n")
  let fields = splitLines sections.[0]
  let myTicket = splitLines sections.[1] |> Array.skip 1 |> Array.head
  let nearbyTickets = splitLines sections.[2] |> Array.skip 1
  { Fields = fields |> Array.map parseField
    MyTicket = myTicket |> parseTicket
    NearbyTickets = nearbyTickets |> Array.map parseTicket
  }

open System
open System.IO
let run () =
  "input/Day16.txt"
  |> File.ReadAllText
  |> parse
  |> part1
  |> Console.WriteLine