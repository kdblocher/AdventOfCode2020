module Day7
open FSharpPlus

type Edge = {
  Value: int
  NodeName: string  
}
type Node = {
  Name: string
  Edges: Edge list
}

type Graph = Node seq
type DirectedGraph = (string, Edge list) Map
module Graph =
  let toDirected : Graph -> DirectedGraph =
    Seq.map (fun node -> node.Name, node.Edges) >> Map.ofSeq 
  let ofDirected : DirectedGraph -> Graph =
    Map.toSeq >> Seq.map (fun (name, edges) -> { Name = name; Edges = edges })

  let transpose : DirectedGraph -> DirectedGraph =
    Map.empty |> Map.fold (fun graph name ->
      graph |> List.fold (fun graph edge ->
        let add existing =
          graph |> Map.add edge.NodeName ({ Value = edge.Value; NodeName = name } :: existing)
        graph
        |> Map.tryFind edge.NodeName
        |> Option.map add
        |> Option.defaultWith (fun () -> add [])))

let rec reach name (graph: DirectedGraph) =
  seq {
    let edges = Map.tryFind name graph |> Option.defaultValue []
    yield name
    yield! edges |> Seq.collect (fun x -> reach x.NodeName graph)
  }
  |> set

let part1 name =
  Graph.transpose
  >> reach name
  >> Seq.length
  >> (fun l -> l - 1)

open System.Text.RegularExpressions
let parseLine input =
  let regex = Regex.Match (input, "^([\w\s]+) bags contain ((\d+)? ?([\w\s]+) bags?[,.]\s?)+$")
  let name = regex.Groups.[1].Value
  let vals = regex.Groups.[3].Captures :> _ seq |> Seq.map (fun c -> c.Value |> int)
  let refs = regex.Groups.[4].Captures :> _ seq |> Seq.map (fun c -> c.Value)
  { Name = name
    Edges =
      Seq.zip vals refs
      |> Seq.filter (fun (v, r) -> r <> "no other")
      |> Seq.map (fun (v, r) ->
        { Value = v
          NodeName = r })
      |> Seq.toList
  }

let parse =
  Seq.map parseLine
  >> Graph.toDirected

open System
open System.IO
let run () =
  "input/Day7.txt"
  |> File.ReadAllLines
  |> parse
  |> part1 "shiny gold"
  |> Console.WriteLine
