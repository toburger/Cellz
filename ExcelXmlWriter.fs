[<AutoOpen>]
module internal Cellz.ExcelXmlWriter

open System.Xml

let writeElement' writeName attributes writeContent (writer:XmlWriter) =
    writeName writer
    for (prefix,localName,ns,value) in attributes do
        writer.WriteAttributeString(prefix,localName,ns,value)
    writeContent writer
    writer.WriteEndElement()
let ns = "urn:schemas-microsoft-com:office:spreadsheet"
let writeWorkbook =
    writeElement'
        (fun writer -> writer.WriteStartElement("Workbook",ns))
        ["xmlns", "ss", null, ns]
let writeElement (name:string) =
    writeElement' (fun writer -> writer.WriteStartElement name)
let writeWorksheet = writeElement "Worksheet" ["ss","Name",ns,"Sheet1"]
let writeTable columnCount rowCount =
    let toString = sprintf "%i"
    writeElement
        "Table"
        ["ss","ExpandedColumnCount",ns,toString columnCount
         "ss","ExpandedRowCount",ns,toString rowCount]
let writeRow = writeElement "Row" []
let writeCell = writeElement "Cell"
let writeFormulaCell styleID formula = writeElement "Cell" (styleID@["ss","Formula",ns,formula])
let writeData dataType = writeElement "Data" ["ss","Type",ns,dataType]
let writeValue (value:'t) (writer:XmlWriter) =  writer.WriteValue value
let writeStringData (value:string) = writeValue value |> writeData "String"
let writeNumberData (value:decimal) = writeValue value |> writeData "Number"
let writeBooleanData (value:bool) =
    value
    |> function true -> "1" | false -> "0"
    |> writeValue
    |> writeData "Boolean"
let writeEmpty (writer:XmlWriter) = ()
let compose (xs:('t -> unit) list) = fun t -> xs |> List.iter (fun x -> x t)

let writeExcelXmlDocument (output:System.IO.Stream) writeContent =
    let settings = XmlWriterSettings(Indent=true)
    let writer = XmlWriter.Create(output, settings)
    writer.WriteStartDocument()
    writer.WriteProcessingInstruction("mso-application","progid=\"Excel.Sheet\"")
    writeContent writer
    writer.WriteEndDocument()
    writer.Close()

let writeCellData (cell:Cell) =
    let style = cell.FontStyle :?> System.Windows.Style
    let styleID =
        Styles.tryFind style
        |> function
           | Some id -> ["ss", "StyleID", ns, string id]
           | None -> []
    let cellWriter =
        if cell.Data.StartsWith("=") then cell |> formatR1C1 |> (writeFormulaCell styleID)
        else writeCell styleID
    match cell.Value with
    | String "" -> ignore
    | String s ->  writeStringData s
    | Number n -> writeNumberData (n.Value)
    | Bool b -> writeBooleanData b
    |> cellWriter

module Array =
    let findLast (f:'T -> bool) (xs:'T []) =
        let mutable index = xs.Length-1
        while index>= 0 && f(xs.[index]) do index <- index - 1
        index

let writeRows (sheet:Sheet) =
    [for row in sheet.Rows do
        let lastIndex = row.Cells |> Array.findLast (fun cell -> cell.Data = "")
        yield
            [for i=0 to lastIndex do yield writeCellData row.Cells.[i]]
            |> compose
            |> writeRow
    ]
    |> compose

let writeFont (font:FontStyle) =
    let black =  System.Windows.Media.Colors.Black
    let attributes =
        (if font.Family <> null then ["ss","FontName",ns,font.Family] else [])
        @ (if font.Bold then ["ss","Bold",ns,"1"] else [])
        @ (if font.Italic then ["ss","Italic",ns,"1"] else [])
        @ (if font.Underline then ["ss", "Underline", ns, "Single"] else [])
        @ (if font.Size <> 11. then ["ss", "Size", ns, font.Size.ToString()] else [])
        @ (if font.Foreground <> black then ["ss", "Color", ns, Color.toHex font.Foreground] else [])
    writeElement "Font" attributes writeEmpty

let writeAlignment (alignment:System.Windows.TextAlignment) =
    let write s = writeElement "Alignment" ["ss", "Horizontal", ns, s] writeEmpty
    match alignment with
    | System.Windows.TextAlignment.Left -> write "Left"
    | System.Windows.TextAlignment.Right -> write "Right"
    | _ -> writeEmpty

let writeInterior (color:System.Windows.Media.Color) =
    if color <> System.Windows.Media.Colors.Transparent
    then writeElement
            "Interior"
            ["ss", "Pattern", ns, "Solid"; "ss", "Color", ns, Color.toHex color]
            writeEmpty
    else writeEmpty

let writeBorders (border:BorderStyle) =
    let thickness = border.Thickness
    if thickness.Top <> 0.0 || thickness.Bottom <> 0.0 || thickness.Left <> 0.0 || thickness.Right <> 0.0
    then
        ["Bottom",thickness.Bottom
         "Left", thickness.Left
         "Right", thickness.Right
         "Top", thickness.Top]
        |> List.filter (fun (_,weight) -> weight <> 0.0)
        |> List.map (fun (name,weight) ->
            writeElement
                "Border"
                ["ss", "Position", ns, name
                 "ss", "LineStyle", ns, "Continuous"
                 "ss", "Weight", ns, weight.ToString()
                 "ss", "Color", ns, Color.toHex border.Color]
                writeEmpty
        )
        |> compose
        |> writeElement "Borders" []

    else writeEmpty

let writeStyle id (style:CellStyle) =
    [
        writeFont style.Font
        writeAlignment style.Font.Alignment
        writeInterior style.Background
        writeBorders style.Border
    ]
    |> compose
    |> writeElement "Style" ["ss","ID",ns,id]

let writeStyles (styles:Map<string,CellStyle>) (writer:XmlWriter) =
    writer.WriteStartElement("Styles")
    let writeStyles = styles |> Map.map writeStyle |> Map.toList |> List.map snd |> compose
    writer |> writeStyles
    writer.WriteEndElement()

let writeAsExcelXml sheet styles output =
    let writeWorksheet =
        writeRows sheet
        |> writeTable sheet.ColumnCount sheet.Rows.Length
        |> writeWorksheet
    let contentWriter (writer:XmlWriter) =
        writeStyles styles writer
        writeWorksheet writer
    contentWriter
    |> writeWorkbook
    |> writeExcelXmlDocument output