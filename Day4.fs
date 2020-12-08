module Day4

open FSharpPlus

type Height =
  | Inches of int
  | Centimeters of int
  | Unitless of int
module Height =
  open System.Text.RegularExpressions
  let parse input =
    let regex = Regex.Match (input, "(\d)+(in|cm)?")
    let value = regex.Groups.[1].Value |> int
    let kind = regex.Groups.[2].Value
    match kind with
    | "in" -> Some <| Inches value
    | "cm" -> Some <| Centimeters value
    | "" -> Some <| Unitless value
    | _ -> None


type Property =
  | BirthYear of Choice<int, string>
  | IssueYear of Choice<int, string>
  | ExpirationYear of Choice<int, string>
  | Height of Choice<Height, string>
  | HairColor of string
  | EyeColor of string
  | PassportId of Choice<int64, string>
  | CountryId of Choice<int, string>

module Property =
  let tryParse f input =
    input
    |> f
    |> function
      | Some v -> Choice1Of2 v
      | None -> Choice2Of2 input

  let tryParsePrimitive f =
    tryParse (f >> fun (success, value) -> if success then Some value else None)

  open System
  let tryParseInt32 (input: string) = input |> tryParsePrimitive Int32.TryParse
  let tryParseInt64 (input: string) = input |> tryParsePrimitive Int64.TryParse

  let parse value =
    function
    | "byr" -> value |> tryParseInt32 |> BirthYear
    | "iyr" -> value |> tryParseInt32 |> IssueYear
    | "eyr" -> value |> tryParseInt32 |> ExpirationYear
    | "hgt" -> value |> tryParse Height.parse |> Height
    | "hcl" -> value |> HairColor
    | "ecl" -> value |> EyeColor
    | "pid" -> value |> tryParseInt64 |> PassportId
    | "cid" -> value |> tryParseInt32 |> CountryId
    | _ -> failwith "invalid key"

[<CLIMutable>]
type Passport = {
  BirthYear: Choice<int, string> option
  IssueYear: Choice<int, string> option
  ExpirationYear: Choice<int, string> option
  Height: Choice<Height, string> option
  HairColor: string option
  EyeColor: string option
  PassportId: Choice<int64, string> option
  CountryId: Choice<int, string> option
}
module Passport =
  let private zero = System.Activator.CreateInstance typeof<Passport> :?> Passport
  let private withProperty p =
    function
    | BirthYear v -> { p with BirthYear = Some v }
    | IssueYear v -> { p with IssueYear = Some v }
    | ExpirationYear v -> { p with ExpirationYear = Some v }
    | Height v -> { p with Height = Some v }
    | HairColor v -> { p with HairColor = Some v }
    | EyeColor v -> { p with EyeColor = Some v }
    | PassportId v -> { p with PassportId = Some v }
    | CountryId v -> { p with CountryId = Some v }

  let validate p =
    match p with
    | { BirthYear = Some _
        IssueYear = Some _
        ExpirationYear = Some _
        Height = Some _
        HairColor = Some _
        EyeColor = Some _
        PassportId = Some _
      } -> Some p
    | _ -> None

  open System.Text.RegularExpressions
  let parse input =
    Regex.Matches (input, "((\w\w\w)\:([^ \r\n]+)\s{0,2})")
    :> _ seq
    |> Seq.map (fun regex ->
      let key = regex.Groups.[2].Value
      let value = regex.Groups.[3].Value
      Property.parse value key)
    |> Seq.fold withProperty zero

  let parseMultiple input =
    Regex.Split (input, "\r\n\r\n")
    |> Seq.map parse

let part1 x =
  x
  |> Seq.choose Passport.validate
  |> Seq.length

open System
open System.IO
let run () =
  "input/Day4.txt"
  |> File.ReadAllText
  |> Passport.parseMultiple
  |> part1
  |> Console.WriteLine

[<EntryPoint>]
let main _ =
  run ()
  0