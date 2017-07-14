namespace Cellz

type colIndex = ColIndex of int with
    static member toColName x =
        let toChar x = ('A' + char x).ToString()
        let rec toStr x =
            if x < 26 then toChar x
            else (toStr ((x/26)-1)) + toChar(x%26)
        toStr x
    override col.ToString () =
        col |> function ColIndex i -> colIndex.toColName i
    static member parse (s:string) =
        let mutable acc = 0
        for i=0 to s.Length-1 do acc <- (acc*26) + int (s.Chars(i)) - int 'A'
        ColIndex(acc)
    static member (+) (ColIndex(lhs),ColIndex(rhs)) = ColIndex(lhs+rhs)
    static member (-) (ColIndex(lhs),ColIndex(rhs)) = ColIndex(lhs-rhs)

type rowIndex = RowIndex of int with
    override row.ToString () =
        row |> function RowIndex i -> sprintf "%i" (i+1)
    static member parse s = RowIndex(System.Int32.Parse(s)-1)
    static member (+) (RowIndex(lhs),RowIndex(rhs)) = RowIndex(lhs+rhs)
    static member (-) (RowIndex(lhs),RowIndex(rhs)) = RowIndex(lhs-rhs)

type address = Address of colIndex * rowIndex with
    override this.ToString () =
        this |> function Address(col,row) -> sprintf "%O%O" col row
    static member parse (s:string) =
        let index = s.IndexOfAny([|'0'..'9'|])
        let col = s.Substring(0,index)
        let row = s.Substring(index)
        Address(colIndex.parse(col),rowIndex.parse(row))

type single =
    | Absolute of address
    | Relative of rowIndex * colIndex
    member single.toAddress (Address(c,r) as fromAddress) =
        single |> function
        | Absolute address -> address
        | Relative(dr,dc) -> Address(c+dc,r+dr)

type reference =
    | Single of single
    | Range of single * single
    static member toList (fromAddress:address) = function
        | Single(Absolute a) -> [a]
        | Single(Relative (dr,dc)) ->
            let c, r = fromAddress |> function Address(c,r) -> c,r
            [Address(c+dc,r+dr)]
        | Range(single1,single2) ->
            let toColRow = function Address(ColIndex(c),RowIndex(r)) -> c,r
            let c1,r1 = single1.toAddress fromAddress |> toColRow
            let c2,r2 = single2.toAddress fromAddress |> toColRow
            let c1, c2 = min c1 c2, max c1 c2
            let r1, r2 = min r1 r2, max r1 r2
            [for c = c1 to c2 do
                for r = r1 to r2 do
                    yield Address(ColIndex(c),RowIndex(r))
            ]
