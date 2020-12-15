module Day4

module Property =
  open System
  open FSharpPlus.Data
  open FSharpPlus.Operators
  open System.Text.RegularExpressions

  let bounded operator directionDescripton description target x =
    if operator x target then Ok x else Error <| sprintf "%s must be %s than %i, but it is %i" description directionDescripton target x

  let atLeast = bounded (>=) "greater"
  let atMost = bounded (<=) "less"
  let between label lower upper = atLeast label lower >=> atMost label upper

  let tryParse f input =
    input
    |> f
    |> function
      | Some v -> Ok v
      | None -> Error input

  let tryParsePrimitive f =
    tryParse (f >> fun (success, value) -> if success then Some value else None)

  open System
  let tryParseInt32 (input: string) = input |> tryParsePrimitive Int32.TryParse
  let tryParseInt64 (input: string) = input |> tryParsePrimitive Int64.TryParse

  type BirthYear = private BirthYear of int with
    static member TryCreate =
      let desc = "Birth year"
      liftM BirthYear <!> (tryParseInt32 >=> between desc 1920 2002)

  type IssueYear = private IssueYear of int with
    static member TryCreate =
      let desc = "Issue year"
      liftM IssueYear <!> (tryParseInt32 >=> between desc 2010 2020)
        
  type ExpirationYear = private ExpirationYear of int with
    static member TryCreate =
      let desc = "Expiration year"
      liftM ExpirationYear <!> (tryParseInt32 >=> between desc 2020 2030)

  type Height =
    private
    | Inches of int
    | Centimeters of int
    with
    static member private Parse input =
      let regex = Regex.Match (input, "(\d+)(in|cm)?")
      let value = regex.Groups.[1].Value |> int
      let kind = regex.Groups.[2].Value
      match kind with
      | "in" -> Ok <| Inches value
      | "cm" -> Ok <| Centimeters value
      | _ -> Error "Could not parse height value"
    static member TryCreate =
      let desc = "Height"
      Height.Parse >=> function
      | Inches i -> i |> (liftM Inches <!> between desc 59 76)
      | Centimeters c -> c |> (liftM Centimeters <!> between desc 150 193)

  type HairColor = private HairColor of int with
    static member private Parse input =
      let regex = Regex.Match (input, "#([0-9a-f]{6})")
      let color = regex.Groups.[1].Value
      match Int32.TryParse (color, Globalization.NumberStyles.HexNumber, Globalization.NumberFormatInfo.InvariantInfo) with
        | true, result -> Ok result
        | _ -> Error "Could not parse hair color value"
    static member TryCreate =
      liftM HairColor <!> HairColor.Parse

  type EyeColor =
    private
    | Amber
    | Blue
    | Brown
    | Grey
    | Green
    | Hazel
    | Other
    with
    static member Parse =
      function
      | "amb" -> Ok Amber
      | "blu" -> Ok Blue
      | "brn" -> Ok Brown
      | "gry" -> Ok Grey
      | "grn" -> Ok Green
      | "hzl" -> Ok Hazel
      | "oth" -> Ok Other
      | _ -> Error "Could not parse eye color value"
    static member TryCreate = 
      EyeColor.Parse
      
  type PassportId = private PassportId of int64 with
    static member private Parse input : Result<_, string> =
      let regex = Regex.Match (input, "^([0-9]{9})$")
      let id = regex.Groups.[1].Value
      match Int64.TryParse id with
        | true, value -> Ok value
        | _ -> Error "Could not parse passport ID"
    static member TryCreate =
      liftM PassportId <!> PassportId.Parse

  type CountryId = private CountryId of string with
    static member TryCreate =
      liftM CountryId <!> (Ok : _ -> Result<_, string>)

  type Property =
  | BirthYearProp of Result<BirthYear, string>
  | IssueYearProp of Result<IssueYear, string>
  | ExpirationYearProp of Result<ExpirationYear, string>
  | HeightProp of Result<Height, string>
  | HairColorProp of Result<HairColor, string>
  | EyeColorProp of Result<EyeColor, string>
  | PassportIdProp of Result<PassportId, string>
  | CountryIdProp of Result<CountryId, string>

  let parse =
    function
    | "byr" -> BirthYearProp <!> BirthYear.TryCreate
    | "iyr" -> IssueYearProp <!> IssueYear.TryCreate
    | "eyr" -> ExpirationYearProp <!> ExpirationYear.TryCreate
    | "hgt" -> HeightProp <!> Height.TryCreate
    | "hcl" -> HairColorProp <!> HairColor.TryCreate
    | "ecl" -> EyeColorProp <!> EyeColor.TryCreate
    | "pid" -> PassportIdProp <!> PassportId.TryCreate
    | "cid" -> CountryIdProp <!> CountryId.TryCreate
    | x -> failwith (sprintf "invalid key: %s" x)

[<CLIMutable>]
type Passport = {
  BirthYear: Result<Property.BirthYear, string> option
  IssueYear: Result<Property.IssueYear, string> option
  ExpirationYear: Result<Property.ExpirationYear, string> option
  Height: Result<Property.Height, string> option
  HairColor: Result<Property.HairColor, string> option
  EyeColor: Result<Property.EyeColor, string> option
  PassportId: Result<Property.PassportId, string> option
  CountryId: Result<Property.CountryId, string> option
}
module Passport =
  let private zero = System.Activator.CreateInstance typeof<Passport> :?> Passport
  let private withProperty p =
    function
    | Property.BirthYearProp v -> { p with BirthYear = Some v }
    | Property.IssueYearProp v -> { p with IssueYear = Some v }
    | Property.ExpirationYearProp v -> { p with ExpirationYear = Some v }
    | Property.HeightProp v -> { p with Height = Some v }
    | Property.HairColorProp v -> { p with HairColor = Some v }
    | Property.EyeColorProp v -> { p with EyeColor = Some v }
    | Property.PassportIdProp v -> { p with PassportId = Some v }
    | Property.CountryIdProp v -> { p with CountryId = Some v }

  let isComplete p =
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

  let isValid p =
    match p with
    | { BirthYear = Some (Ok _)
        IssueYear = Some (Ok _)
        ExpirationYear = Some (Ok _)
        Height = Some (Ok _)
        HairColor = Some (Ok _)
        EyeColor = Some (Ok _)
        PassportId = Some (Ok _)
      } -> Some p
    | _ -> None

  open System.Text.RegularExpressions
  let parse input =
    Regex.Matches (input, "((\w\w\w)\:([^ \r\n]+)\s{0,2})")
    :> _ seq
    |> Seq.map (fun regex ->
      let key = regex.Groups.[2].Value
      let value = regex.Groups.[3].Value
      Property.parse key value)
    |> Seq.fold withProperty zero

  let parseMultiple input =
    Regex.Split (input, "\r\n\r\n")
    |> Seq.map parse

let part1 : Passport seq -> int =
  Seq.choose Passport.isComplete
  >> Seq.length

let part2 : Passport seq -> int =
  Seq.choose Passport.isValid
  >> Seq.length

open System
open System.IO
let run () =
  "input/Day4.txt"
  |> File.ReadAllText
  |> Passport.parseMultiple
  |> part2
  |> Console.WriteLine

[<EntryPoint>]
let main _ =
  run ()
  0