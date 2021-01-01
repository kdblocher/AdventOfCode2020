module Day17
open System

module List =
  let cons a b = a :: b

let update count active =
  (active && (count = 2 || count = 3))
  || (not active && count = 3)

let product bounds =
  Seq.foldBack (fun (a, b) s ->
    seq { a .. b }
    |> (if Seq.isEmpty s
      then Seq.map List.singleton
      else Seq.allPairs s >> Seq.map (fun (ls, x) -> List.cons x ls))) bounds Seq.empty

let cycleOnce active =
  let dim =
    active
    |> Seq.head
    |> List.length

  let bound i = 
    active
    |> Set.toSeq
    |> Seq.map (List.skip i >> List.head)
    |> fun d -> Seq.min d, Seq.max d

  let bounds =
    List.init dim bound
    |> Seq.map (fun (a, b) -> a - 1, b - a + 2)
    |> product
  
  let neighbors =
    List.init dim (fun _ -> -1, 1)
    |> product
    |> Seq.filter (not << List.forall ((=) 0))
  
  let counts =
    bounds
    |> Seq.map (fun pos ->
      pos,
      neighbors
      |> Seq.map (List.map2 (+) pos)
      |> Seq.filter (fun d -> active |> Set.contains d)
      |> Seq.length)
  
  counts
  |> Seq.fold (fun active' (pos, count) ->
    if active |> Set.contains pos |> update count
    then Set.add pos active'
    else active')
    Set.empty

let cycle count =
  let rec loop i active =
    if i = count then active else loop (i + 1) (cycleOnce active)
  loop 0

let init dim =
  Seq.map (fun pos -> List.append pos (List.init (dim - 2) (fun _ -> 0)))
  >> set

let part1 : _ seq -> _ =
  init 3
  >> cycle 6
  >> Set.count

let part2 : _ seq -> _ =
  init 4
  >> cycle 6
  >> Set.count

let parseChar =
  function
  | '.' -> false
  | '#' -> true
  | _ -> failwith "Invalid cube character"

let parseLine : _ seq -> _ =
  Seq.map parseChar

let parse =
  Seq.map parseLine
  >> Seq.mapi (fun x ->
    Seq.mapi (fun y c ->
      c, [x; y]))
  >> Seq.collect id
  >> Seq.choose (fun (cube, pos) -> if cube then Some pos else None)
 
open System.IO
let run () =
  "input/Day17.txt"
  |> File.ReadAllLines
  |> parse
  |> part2
  |> Console.WriteLine