module Day16

module List =
  let cons a b = a :: b
module Tuple =
  let flip (a, b) = b, a
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

let sortRanges = Array.sortBy (fun r -> r.Lower, r.Upper)

let consolidate =
  let rec scan =
    function
    | [] -> []
    | [r] -> [r]
    | r1 :: r2 :: ranges ->
      match combine (r1, r2) with
      | Some r -> r :: scan ranges
      | None -> r1 :: scan (r2 :: ranges)
  sortRanges
  >> List.ofArray
  >> scan
  >> List.toArray

let inRange v r =
  v >= r.Lower && v <= r.Upper


let inline validForAnyRange fields  =
  let ranges =
    fields
    |> Array.collect (fun f -> f.Ranges)
    |> consolidate
    |> Array.toSeq
  fun v -> ranges |> Seq.exists (inRange v)

let part1 notes =
  let validate = validForAnyRange notes.Fields
  notes.NearbyTickets
  |> Seq.collect id
  |> Seq.filter (not << validate)
  |> Seq.sum

let candidateFields fields v =
  fields
  |> Array.filter (fun f -> validForAnyRange (Array.singleton f) v)
  |> set

let candidateFieldsForPosition fields =
  Seq.map (candidateFields fields) >> Set.intersectMany

let allCandidatePositions (fields: Field array) validTickets =
  let length = validTickets |> Seq.head |> Array.length
  { 0 .. length - 1 }
  |> Seq.map (fun i ->
    validTickets
    |> Seq.map (fun t -> Array.get t i)
    |> candidateFieldsForPosition fields)
  |> Seq.indexed
  |> Map

let constraintSolve =
  let rec loop constraints =
    if   Map.isEmpty constraints then Some []
    elif constraints |> Map.exists (fun _ -> Set.isEmpty) then None
    else
      match constraints |> Map.tryFindKey (fun _ s -> s |> Set.count |> (=) 1) with
      | Some k ->
        let e = constraints.[k] |> Set.toSeq |> Seq.head
        constraints
        |> Map.map (fun _ -> Set.remove e)
        |> Map.remove k
        |> loop
        |> Option.map (List.cons (k, e))
      | _ -> failwith "backtracking not implemented"
  loop >> Option.map (Seq.map Tuple.flip >> Map)

let part2 notes =
  notes.NearbyTickets
  |> Seq.append (Seq.singleton notes.MyTicket)
  |> Seq.filter (Array.forall (validForAnyRange notes.Fields))
  |> allCandidatePositions notes.Fields
  |> constraintSolve
  |> Option.get
  |> Map.filter (fun k _ -> k.Name.StartsWith "departure")
  |> Map.toSeq
  |> Seq.map (snd >> (fun i -> notes.MyTicket.[i] |> int64))
  |> Seq.fold (*) 1L

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
    |] |> sortRanges
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
  |> part2
  |> Console.WriteLine