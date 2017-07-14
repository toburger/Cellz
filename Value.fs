namespace Cellz

type value =
    | Number of UnitValue
    | String of string
    | Bool of bool
    static member empty = String ""
    member value.Value = value |> function
        | Number n -> box n
        | String s -> box s
        | Bool b -> box b
    static member toUnitValue = function
        | Number n -> n
        | String _ -> UnitValue(0M)
        | Bool _ -> UnitValue(0M)
    override value.ToString() =
        value.Value.ToString()
