module Day18

type Operator = Add | Multiply
type Exp =
  | Number of int64
  | Operation of Exp * Operator * Exp

module Parse =
  open FParsec
  type 'a Parser = ('a, unit) Parser

  let parseNum : _ Parser = pint64 .>> spaces |>> Number

  let private opp = new OperatorPrecedenceParser<_,_,_>()

  let private infixOp op map =
    opp.AddOperator (InfixOperator (op, spaces, 1, Associativity.Left, map))

  infixOp "+" (fun x y -> Operation (x, Add, y))
  infixOp "*" (fun x y -> Operation (x, Multiply, y))

  let private parseExp = opp.ExpressionParser

  let pad s = pchar s >>. spaces
  let private parseParens =
    between (pad '(') (pad ')') parseExp

  opp.TermParser <-
    parseParens
    <|> parseNum
    
  let parseLine =
    parseExp .>> eof

  let run =
    runParserOnString parseLine () ""
    >> function Success (a, _, _) -> a | Failure _ -> failwith "parse failure"

let rec eval =
  function
  | Number x -> x
  | Operation (a, op, b) ->
    (match op with Add -> (+) | Multiply -> (*)) (eval a) (eval b)

let part1 : _ seq -> _ =
  Seq.sumBy (Parse.run >> eval)

open FParsec
open System
open System.IO
let run () =
  "input/Day18.txt"
  |> File.ReadAllLines
  |> part1
  |> Console.WriteLine