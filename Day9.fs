module Day9

module Seq =
  open FSharpPlus
  let foldWhile folder state =
    Ok state
    |> Seq.scan (fun state e -> 
      state >>= (fun state -> folder state e))
    >> Seq.pairwise
    >> Seq.takeWhile (fun (a, b) -> match a with Ok _ -> true | _ -> false)
    >> Seq.last
    >> snd

module CircularBuffer =

  type Buffer = private {
    Head: int
    Numbers: int64 array
    Sums: int64 option array array
  }

  let private calcSums head next =
    Array.mapi (fun j m -> if head = j then None else Some (next + m))

  let create preamble =
    let preamble = preamble |> Seq.toArray
    { Head = preamble.Length - 1
      Numbers = preamble
      Sums =
        preamble |> Array.mapi (fun head next ->
          preamble |> calcSums head next)
    }

  let rotate next { Head = head; Numbers = numbers; Sums = sums } =
    let head = (head + 1) % numbers.Length
    numbers.[head] <- next
    sums.[head] <- numbers |> calcSums head next
    { Head = head
      Numbers = numbers
      Sums = sums
    }

  let check next { Sums = sums } =
    sums
    |> Array.collect id
    |> Array.exists (Option.exists ((=) next))

  let tryAdd buffer next =
    if check next buffer
    then Ok <| rotate next buffer
    else Error <| next

let part1 preamble =
  CircularBuffer.create preamble
  |> Seq.foldWhile CircularBuffer.tryAdd
  >> function
    | Error number -> number
    | Ok _ -> failwith "unexpected"

let part2 preamble (input: int64 array) =
  let key = part1 preamble input
  let rec loop head tail sum =
    if sum = key then
      let nums = Array.sub input head (tail - head) 
      Array.min nums + Array.max nums
    else if sum > key then loop (head + 1) tail (sum - input.[head])
    else if sum < key then loop head (tail + 1) (sum + input.[tail])
    else failwith "unexpected"
  loop 0 0 0L

let parse preambleLength =
  Array.map int64
  >> Array.splitAt preambleLength

open System
open System.IO
let run () =
  "input/Day9.txt"
  |> File.ReadAllLines
  |> parse 25
  |> (fun (preamble, input) -> part2 preamble input)
  |> Console.WriteLine