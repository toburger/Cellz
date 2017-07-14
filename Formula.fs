[<AutoOpen>]
module internal Cellz.Formula
open Cellz.ParserCombinators
open Cellz.Tokenizer

type expr =
    | Value of value
    | Reference of reference
    | Operator of operator * expr * expr
    | Function of string * expr list
    static member empty = String "" |> Value
    member expr.References (address:address) =
        let rec traverse = function
            | Value _ -> []
            | Reference r -> reference.toList address r
            | Function(_,es) -> es |> List.collect traverse
            | Operator(_,e,e') -> traverse e @ traverse e'
        traverse expr
    member expr.Evaluate (address:address) (lookup:address -> value) (invoke:string -> value list -> value) =
        let op f v v' =
            f (value.toUnitValue v) (value.toUnitValue v') |> Number
        let cmp f v v' =
            f (value.toUnitValue v) (value.toUnitValue v') |> Bool
        let rec eval = function
            | Value value -> value
            | Reference(Single(Absolute a)) -> lookup a
            | Reference(Single(Relative(dr,dc))) ->
                let c,r = address |> function Address(c,r) -> c,r
                lookup (Address(c+dc,r+dr))
            | Reference(Range(a,a')) -> invalidOp "Ranges can only be evaluated by functions"
            | Operator(Plus,e,e') -> op (+) (eval e) (eval e')
            | Operator(Minus,e,e') -> op (-) (eval e) (eval e')
            | Operator(Multiply,e,e') -> op (*) (eval e) (eval e')
            | Operator(Divide,e,e') -> op (/) (eval e) (eval e')
            | Operator(Power,e,e') -> op ( ** ) (eval e) (eval e')
            | Operator(Equals,e,e') -> cmp (=) (eval e) (eval e')
            | Function(name,es) -> invoke name (es |> List.collect toArgs)
        and toArgs e =
            match e with
            | Reference(Range(_,_) as range) ->
                range |> reference.toList address |> List.map lookup
            | e -> [eval e]
        eval expr

let (|Int|_|) (n:UnitValue) =
    let s = n.Value.ToString()
    match System.Int32.TryParse(n.Value.ToString()) with
    | true, n -> Some n
    | false,_ -> None

let toValue = function
    | token.Operator(Minus) :: (token.Value(Number(n))::t) -> Some(Value(Number(-n)), t)
    | token.Value(Number(v))::token.Name name::token.Operator(Power)::token.Value(Number(Int n))::t ->
        Some(Value(Number(UnitValue(v.Value,Unit(name,n)))),t)
    | token.Name name::token.Operator(Power)::token.Value(Number(Int n))::t ->
        Some(Value(Number(UnitValue(1.0M,Unit(name,n)))),t)
    | token.Name name :: t -> Some(Value(Number(UnitValue(1.0M,name))),t)
    | token.Value(Number(v))::token.Name name::t ->
        Some(Value(Number(UnitValue(v.Value,name))),t)
    | token.Value v :: t -> Some(Value v, t)
    | _ -> None

let toReference = function token.Reference r :: t-> Some(Reference r,t) | _ -> None

let name = function token.Name name -> true | _ -> false

let (|Func|) = function
    | (token.Name(name),_),_ -> Function(name,[])
    | _ -> invalidOp ""

let (|FuncWithArgs|) = function
    | (((token.Name(name),_),ps), p), _ -> Function(name,(ps |> List.map fst) @ [p])
    | _ -> invalidOp ""

let sum = function
    | token.Operator Plus | token.Operator Minus -> true
    | _ -> false

let product = function
    | token.Operator Multiply | token.Operator Divide | token.Operator Power -> true
    | _ -> false

let comparison = function
    | token.Operator Equals -> true
    | _ -> false

let (|Op|) =
    function ((lhs,token.Operator(op)),rhs) -> Operator(op,lhs,rhs) | _ -> invalidOp ""

let rec ops x = function
    | [] -> x
    | (token.Operator(op),e)::xs -> ops (Operator(op,x,e)) xs
    | _ -> invalidOp ""

let rec atom s =
    ((toReference >| fun r -> r) |||
     (a Open ++ term ++ a Close >| fun ((_, e), _) -> e) |||
     (some name ++ a Open ++ a Close >| function Func f -> f) |||
     (some name ++ a Open ++ many (term ++ a Seperator) ++ term ++ a Close >| function FuncWithArgs f -> f) |||
     (toValue >| fun v -> v)
    ) s
and factor s =
    ((atom ++ many (some product ++ atom) >| function (x,xs) -> ops x xs) |||
     (atom |> fun e -> e)
    ) s
and sums s =
    ((factor ++ many (some sum ++ factor) >| function (x,xs) -> ops x xs) |||
     (factor |> fun e -> e)) s
and term s =
    ((sums ++ some comparison ++ sums >| function Op op -> op) |||
     (sums |> fun e -> e)) s

let (|Decimal|_|) s =
    match System.Decimal.TryParse s with
    | true, d -> d |> Some
    | false, _ -> None

let (|Boolean|_|) s =
    match System.Boolean.TryParse s with
    | true, b -> b |> Some
    | false, _ -> None

let parse (s:string) =
    if s.StartsWith("=") then
        let x = tokenize (s.Substring(1)) |> term
        x |> function Some(e,[]) -> e | _ -> invalidOp "Failed to parse"
    else
        match s with
        | Decimal d -> Number (UnitValue(d))
        | Boolean b -> Bool b
        | _ -> String s
        |> Value