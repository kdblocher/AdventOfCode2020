module Day13
open FSharpPlus.Math.Generic

module Math =

  // Extended Euclidian algorithm to compute Bezout's coefficients
  // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
  let xgcd a b =
    let rec loop (r, s, t) (r', s', t') =
      if r' = bigint 0
      then r, s, t
      else 
        let q = r / r'
        let r, r' = r', r - q * r'
        let s, s' = s', s - q * s'
        let t, t' = t', t - q * t'
        loop (r, s, t) (r', s', t')
    loop (a, bigint 1, bigint 0) (b, bigint 0, bigint 1)

  // Solve Chinese remainder theorem with two equations
  // https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Existence_(constructive_proof)
  let crt2 (a1, n1) (a2, n2) =
    let gcd, m1, m2 = xgcd n1 n2
    let x = (a1 * m2 * n2) + (a2 * m1 * n1)
    let n' = n1 * n2
    remE x n', n'

  // General case
  let crt =
    Seq.reduce crt2
    >> fst

type Bus =
  | OutOfService
  | Id of int

let getId = function OutOfService -> None | Id x -> Some x

let part1 time =
  Seq.choose getId
  >> Seq.map (fun b -> b, b - (time % b))
  >> Seq.minBy snd
  >> (fun (a, b) -> a * b)

open FSharpPlus
let part2 _ =
  Seq.indexed
  >> Seq.choose (fun (index, bus) ->
    (fun id -> id - index, id) // 'a' is the index minus id, and 'n' is the id
    <!> getId bus)
  >> Seq.map (fun (a, n) -> bigint a, bigint n)
  >> Math.crt

open System.Text.RegularExpressions
let parse (input: string array) =
  let time = input.[0] |> int
  let buses =
    input.[1]
    |> fun x -> Regex.Split (x, ",")
    |> Seq.map (function "x" -> OutOfService | x -> Id <| int x)
  time, buses

open System
open System.IO
let run () =
  "input/Day13.txt"
  |> File.ReadAllLines
  |> parse
  |> fun (time, buses) -> part2 time buses
  |> Console.WriteLine