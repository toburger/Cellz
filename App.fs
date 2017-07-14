namespace Cellz

open System.Windows
open System.Windows.Controls
open System.Windows.Media

[<AutoOpen>]
module Stats =
    let LOC =
        ["Name"             ,"LOC"
         "ObservableObject" ,"18"
         "ParserCombinators","35"
         "Value"            ,"17"
         "Reference"        ,"60"
         "Tokenizer"        ,"143"
         "Formula"          ,"117"
         "Functions"        ,"18"
         "Cell"             ,"120"
         "CellFormat"       ,"36"
         "ExcelXmlWriter"   ,"81"
         "ExcelXmlReader"   ,"142"
         "MessageWindow"    ,"17"
         "View"             ,"64"
         "Frame"            ,"26"
         "App"              ,"59"
         "Total"            ,"=SUM(B2:B16)"
         "Average"          ,"=AVERAGE(B2:B16)"
        ]
    let colCount, rowCount = 30, 30
    let sheet = Sheet(colCount,rowCount,Functions.invoke)
    do  sheet.Columns.[0].Width <- 128.0
        sheet.Columns.[1].Width <- 64.0
        LOC
        |> List.iteri (fun i (name,loc) ->
            sheet.CellAt(ColIndex(0),RowIndex(i)).Data <- name
            sheet.CellAt(ColIndex(1),RowIndex(i)).Data <- loc
        )
        let setThickness(col,row) thickness =
            let cell = sheet.CellAt(ColIndex(col),RowIndex(row))
            let border = { Color = Colors.Black; Thickness = thickness }
            cell.BorderStyle <- border.ToStyle()
        let setBackground(col,row) color =
            let cell = sheet.CellAt(ColIndex(col),RowIndex(row))
            cell.Background <- SolidColorBrush color
        for i in 1..15 do
            setBackground(0,i) Colors.LightGray
            setBackground(1,i) Colors.LightGray
        let top = Thickness(0.0,0.0,0.0,3.0)
        setThickness (0,0) top
        setThickness (1,0) top
        setBackground (0,0) Colors.Orange
        setBackground (1,0) Colors.Orange
        let bottom = Thickness(0.0,3.0,0.0,0.0)
        setThickness (0,16) bottom
        setThickness (1,16) bottom
        setBackground (0,16) Colors.Yellow
        setBackground (1,16) Colors.Yellow
        let topbot = Thickness(0.0,1.0,0.0,3.0)
        setThickness (0,17) topbot
        setThickness (1,17) topbot
        setBackground (0,17) Colors.Orange
        setBackground (1,17) Colors.Orange

#if SILVERLIGHT
type App() as app =
    inherit Application()
    let frame = Frame(sheet)
    do app.Startup.AddHandler(fun o e -> app.RootVisual <- frame)
#else
module App =
    [<System.STAThread;EntryPoint>]
    let main _ =
        let win = Window(Title="Cellz", Content=Cellz.Frame(sheet))
        (System.Windows.Application()).Run(win)
#endif