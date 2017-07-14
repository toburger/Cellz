[<AutoOpen>]
module internal Cellz.CellFormat

let format (fromAbsolute,fromRelative) (expr:expr) =
    let rec toString = function
        | Value(String s) -> sprintf "\"%s\"" s
        | Value(Number n) -> n.ToString()
        | Value(Bool(true)) -> "TRUE"
        | Value(Bool(false)) -> "FALSE"
        | Reference(Single(Absolute(address))) -> fromAbsolute address
        | Reference(Single(Relative(dr,dc))) -> fromRelative (dr,dc)
        | Reference(Range(address,address')) ->
            let tos = function Absolute a -> fromAbsolute a | Relative(r1,c1) -> fromRelative (r1,c1)
            sprintf "%s:%s" (tos address) (tos address')
        | Operator(op,lhs,rhs) ->
            sprintf "%s%s%s" (toString lhs) (op.ToString()) (toString rhs)
        | Function(name,args) ->
            let args = String.concat "," (args |> List.map toString)
            sprintf "%s(%s)" name args
    sprintf "=%s" (toString expr)

let formatR1C1 (cell:Cell) =
    let c,r = cell.Address |> function (Address(c,r)) -> c,r
    let toR1C1 (RowIndex(dr),ColIndex(dc)) =
        let toN = function 0 -> "" | n -> sprintf "[%i]" n
        sprintf "R%sC%s" (toN dr) (toN dc)
    let toRelative (Address(c',r')) =
        toR1C1 (r' - r, c' - c)
    format (toRelative, toR1C1) cell.Expression

let formatA1 (Address(c,r)) (expr:expr) =
    let toAddress (dr,dc) =
        let address = Address(c+dc,r+dr)
        address.ToString()
    let ofAddress address = address.ToString()
    format (ofAddress, toAddress) expr

