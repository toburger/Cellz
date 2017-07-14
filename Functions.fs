module Cellz.Functions

let invoke name values =
    let toDecimals = List.map value.toUnitValue >> List.map (fun x -> x.Value)
    let numberCount = function Number _ -> 1M | _ -> 0M
    let toBoolean = function Bool x -> x | _ -> false
    match name,values with
    | "SUM",_ -> UnitValue(values |> toDecimals |> List.sum) |> Number
    | "COUNT",_ -> UnitValue(values |> List.sumBy numberCount) |> Number
    | "AVERAGE",_ -> UnitValue(values |> toDecimals |> List.average) |> Number
    | "IF",[Bool condition;t;f] -> if condition then t else f
    | "AND",conditions ->
        conditions |> List.forall toBoolean |> Bool
    | "OR",conditions ->
        conditions |> List.exists toBoolean |> Bool
    | "NOT",[Bool condition] -> not condition |> Bool
    | "PI",[] -> UnitValue(System.Math.PI |> decimal) |> Number
    | _ ->
        let message = "Function not supported"
        new System.NotSupportedException(message) |> raise