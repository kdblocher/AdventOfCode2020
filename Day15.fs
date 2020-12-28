module Day15

type State = {
  Turn: int
  LastSpoken: int
  Log: (int, int) Map
}

let speak state =
  state.Log
  |> Map.tryFind state.LastSpoken
  |> Option.map (fun turn -> state.Turn - turn)
  |> Option.defaultValue 0

let update state lastSpoken =
  { Turn = state.Turn + 1
    LastSpoken = lastSpoken
    Log = state.Log |> Map.add state.LastSpoken (state.Turn)
  }

let turn state =
  speak state |> update state

let init =
  List.scan update { Turn = 0; LastSpoken = 0; Log = Map.empty }

let play nums =
  let starting = init nums
  List.last starting
  |> Seq.unfold (fun state ->
    let state = speak state |> update state
    Some (state, state))
  |> Seq.append starting
  |> Seq.skip 1

let solve turn =
  play
  >> Seq.skip (turn - 1)
  >> Seq.head
  >> (fun state -> state.LastSpoken)

let input =     [ 20;0;1;11;6;3 ]

let part1 () =
  solve 2020 input

let part2 () =
  solve 30000000 input

open System
let run () =
  part2 ()
  |> Console.WriteLine