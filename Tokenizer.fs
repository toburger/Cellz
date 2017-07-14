module internal Cellz.Tokenizer
open Cellz

type operator =
    | Plus | Minus | Multiply | Divide | Power
    | Equals
    with
    override op.ToString() = op |> function
        | Plus -> "+" | Minus -> "-" | Multiply -> "*" | Divide -> "/"
        | Power -> "^"
        | Equals -> "="

type token =
    | Open
    | Close
    | Seperator
    | Value of value
    | Reference of reference
    | Operator of operator
    | Name of string
    | WhiteSpace

let isCharToken token c (s:string) =
    if s.[0] = c then Some(token,1) else None
let (|CharToken|_|) token c = isCharToken token c
let (|OpenToken|_|) = isCharToken Open '('
let (|CloseToken|_|) = isCharToken Close ')'
let (|SeperatorToken|_|) = isCharToken Seperator ','

let (|OperatorToken|_|) (s:string) =
    match s.[0] with
    | '+' -> Some Plus
    | '-' -> Some Minus
    | '*' -> Some Multiply
    | '/' -> Some Divide
    | '^' -> Some Power
    | '=' -> Some Equals
    | _ -> None
    |> Option.map (fun op -> Operator(op),1)

open System.Text.RegularExpressions

let tryMatch input pattern =
    let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
    if m.Success then
        [for i = 1 to m.Groups.Count-1 do yield m.Groups.[i].Value]
        |> Some
    else None

let tryRegex input pattern =
    tryMatch input pattern
    |> Option.map (fun xs -> xs.[0])

let (|BoolToken|_|) (s:string) =
    tryRegex s @"^(true|false)"
    |> Option.map (fun b -> System.Boolean.Parse(b) |> Bool |> Value,b.Length)

let (|NumberToken|_|) (s:string) =
    if not <| System.Char.IsDigit(s.[0]) then None
    else
    tryRegex s @"^(\d+(\.\d+)?|\.\d+)"
    |> Option.map (fun ds -> UnitValue(System.Decimal.Parse(ds)) |> Number |> Value,ds.Length)

let (|NameToken|_|) (s:string) =
    tryRegex s @"^([a-z]+)"
    |> Option.map (fun name -> Name name,name.Length)

let (|WhiteSpaceToken|_|) (s:string) =
    let mutable index = 0
    while index < s.Length && System.Char.IsWhiteSpace(s.[index]) do
        index <- index + 1
    if index > 0 then Some(WhiteSpace,index) else None

let (|AddressToken|_|) (s:string) =
    tryRegex s @"^([a-z]+\d+)"
    |> Option.map (fun s -> address.parse s |> Absolute |> Single |> Reference, s.Length)

let parseOffset (s:string) =
    let startIndex, endIndex = s.IndexOf('['),s.IndexOf(']')
    if startIndex = -1 then 0
    else
        let digits = s.Substring(startIndex+1,endIndex-startIndex-1)
        System.Int32.Parse(digits)

let (|R1C1AddressToken|_|) (s:string) =
    tryMatch s @"^(R\[-?\d+\]|R)(C\[-?\d+\]|C)"
    |> Option.map (fun xs ->
        let r = parseOffset xs.[0]
        let c = parseOffset xs.[1]
        let length = xs |> Seq.sumBy (fun x -> x.Length)
        Relative(RowIndex(r),ColIndex(c)) |> Single |> Reference, length
    )

let (|R1C1RangeToken|_|) (s:string) =
    tryMatch s @"^(R\[-?\d+\]|R)(C\[-?\d+\]|C):(R\[-?\d+\]|R)(C\[-?\d+\]|C)"
    |> Option.map (fun xs ->
        let r = parseOffset xs.[0]
        let c = parseOffset xs.[1]
        let s1 = Relative(RowIndex(r),ColIndex(c))
        let r' = parseOffset xs.[2]
        let c' = parseOffset xs.[3]
        let s2 = Relative(RowIndex(r'),ColIndex(c'))
        let length = xs |> Seq.sumBy (fun x -> x.Length)
        Range(s1,s2) |> Reference, length + 1
    )

let (|RangeToken|_|) (s:string) =
    tryRegex s @"^([a-z]+\d+:[a-z]+\d+)"
    |> Option.map (fun s ->
        let index = s.IndexOf(":")
        let r, r' = s.Substring(0,index), s.Substring(index+1)
        let parse s = address.parse s
        Range(Absolute(parse r),Absolute(parse r')) |> Reference, s.Length
    )

let (|StringLiteralToken|_|) (s:string) =
    tryRegex s "^\"([^\"]*)\""
    |> Option.map (fun s -> String s |> Value, s.Length+2)

let toToken = function
    | WhiteSpaceToken t
    | OpenToken t
    | CloseToken t
    | SeperatorToken t
    | OperatorToken t
    | BoolToken t
    | StringLiteralToken t
    | RangeToken t
    | R1C1RangeToken t
    | AddressToken t
    | R1C1AddressToken t
    | NumberToken t
    | NameToken t
        -> t
    | _ -> invalidOp "No matching token found"

let rec tokenize' index s =
    if index = String.length s then []
    else
        let token, length = s.Substring index |> toToken
        token :: tokenize' (index+length) s

let tokenize s =
    tokenize' 0 s |>
    List.choose (function WhiteSpace -> None | t -> Some t)