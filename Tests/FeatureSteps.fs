module FeatureSteps

open Cellz
open TickSpec
#if SILVERLIGHT
open Microsoft.VisualStudio.TestTools.UnitTesting
#else
open NUnit.Framework
#endif

let mutable sheet : Sheet = null
let mutable error = null
let mutable subscription : System.IDisposable = null

let [<Given>] ``a sheet with data:`` (table:Table) =
    sheet <- new Sheet(table.Header.Length, table.Rows.Length)
    if subscription <> null then subscription.Dispose()
    subscription <-
        sheet.Error
        |> Observable.subscribe (fun message -> error <- message)
    let header = table.Header
    for row in table.Rows do
        let rowIndex = rowIndex.parse row.[0]
        for col = 1 to header.Length - 1 do
            let colIndex = colIndex.parse header.[col]
            let data = row.[col]
            sheet.CellAt(colIndex,rowIndex).Data <- data

let [<When>] ``the cell values are computed`` () =
    ()

let [<When>] ``the values are changed:`` (table:Table) =
    error <- null
    for row in table.Rows do
        let address = address.parse row.[0]
        let newValue = row.[1]
        sheet.CellAt(address).Data <- newValue

let [<Then>] ``the values should equal:`` (table:Table) =
    for row in table.Rows do
        let address = address.parse row.[0]
        let expectedValue = row.[1]
        let actualValue = sheet.CellAt(address).Value.ToString()
        Assert.AreEqual(expectedValue,actualValue)

let [<Then>] ``an error notification is received`` () =
    Assert.IsNotNull error