namespace Cellz

type Cell (sheet:Sheet,address:address) as cell =
    inherit ObservableObject()
    let maxGeneration = 1000
    let mutable references = []
    let mutable expr = expr.empty
    let mutable data = ""
    let mutable value = value.empty
    let mutable background : obj = null
    let mutable borderStyle : obj = null
    let mutable fontStyle : obj = null
    let updated = Event<_>()
    let update newValue generation =
        value <- newValue
        cell.NotifyPropertyChanged <@ cell.Value @>
        updated.Trigger generation
    let cellAt (Address(col,row)) : Cell = sheet.CellAt(col,row)
    let valueAt address = (cellAt address).Value
    let invoke (name:string) (args:value list) = sheet.Invoke name args
    let triggerError (e:exn) = e.Message |> sheet.TriggerError
    let eval () =
        try expr.Evaluate address valueAt invoke with
        e -> triggerError e; String "N/A"
    let detectCircularReference () =
        let refs x = (cellAt x).References
        let rec exists n xs =
            if n > maxGeneration || List.exists ((=) address) xs
            then true
            else
                xs |> List.exists (fun x -> exists (n+1) (refs x))
        exists 0 references
    let mutable subscriptions : System.IDisposable list = []
    let subscribe () =
        subscriptions |> List.iter (fun d -> d.Dispose())
        subscriptions <- []
        let remember x = subscriptions <- x :: subscriptions
        for address in references do
            let cell' : Cell = cellAt address
            cell'.Updated
            |> Observable.subscribe (fun generation ->
                let newValue = eval ()
                if generation < maxGeneration then
                    update newValue (generation+1)
            ) |> remember
    member cell.Data
        with get () = data
        and set value =
            data <- value
            expr <-
                try parse value
                with e -> triggerError e; Value(String "N/A")
            references <- expr.References address
            if detectCircularReference() then
                sheet.TriggerError "Circular reference detected"
            subscribe ()
            cell.NotifyPropertyChanged <@ cell.Data @>
            let newValue = eval ()
            update newValue 0
    member cell.Value
        with get () = value
        and set newValue =
            if newValue <> value then
                update newValue 0
    member cell.Formula =
        match expr with
        | Value _ -> null
        | _ -> data
    member cell.Updated = updated.Publish
    member cell.References = references
    override cell.ToString() = value |> sprintf "%A"
    member cell.Background
        with get () = background
        and set value = background <- value
    member cell.BorderStyle
        with get () = borderStyle
        and set value = borderStyle <- value
    member cell.FontStyle
        with get () = fontStyle
        and set value = fontStyle <- value
    member internal cell.Address = address
    member internal cell.Expression = expr
and Row (rowIndex, cells) =
    member row.Index = rowIndex
    member row.Cells = cells
and Column (width) =
    let mutable width = width
    member col.Width
        with get () = width
        and set value = width <- value
and [<AllowNullLiteral>]
    Sheet internal (colCount,rowCount,invoker:string -> value list -> value) as sheet =
    let cols = [|for col=1 to colCount do yield Column(64.0)|]
    let rows =
        [|for row = 0 to rowCount-1 do
            let cells =
                [|for col=0 to colCount-1 do
                    let address = Address(ColIndex(col),RowIndex(row))
                    yield Cell(sheet,address)
                |]
            yield Row(RowIndex(row),cells)
        |]
    let error = Event<_>()
    new (colCount,rowCount) = Sheet(colCount,rowCount,Functions.invoke)
    member sheet.Columns = cols
    member sheet.Rows = rows
    member sheet.ColumnCount = colCount
    member sheet.CellAt(ColIndex(col),RowIndex(row)) = rows.[row].Cells.[col]
    member sheet.CellAt(Address(colIndex,rowIndex)) = sheet.CellAt(colIndex,rowIndex)
    member sheet.Error = error.Publish
    member sheet.TriggerError(s:string) = error.Trigger s
    member sheet.Invoke = invoker
