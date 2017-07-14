[<AutoOpen>]
module internal Cellz.ExcelXmlReader

open System
open System.IO
open System.Xml
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Media

let ns = "urn:schemas-microsoft-com:office:spreadsheet"
let (?) (reader:XmlReader) name = reader.GetAttribute(name,ns)

module Borders =
    let toThickness borders =
        borders |> List.fold (fun ((l,t,r,b),c) border ->
            match border with
            | "Left",x,c -> (x,t,r,b),c
            | "Top",x,c -> (l,x,r,b),c
            | "Right",x,c -> (l,t,x,b),c
            | "Bottom",x,c -> (l,t,r,x),c
            | _ -> invalidOp ""
        ) ((0.0,0.0,0.0,0.0),Colors.Transparent)
        |> fun ((l,t,r,b),c) -> Thickness(l,t,r,b), c

let readStyles (reader:XmlReader) =
    let mutable styles = Map.empty
    let mutable id = ""
    let mutable background = None
    let mutable borders = []
    let mutable font = FontStyle.Default
    let toColor (hex:string) = hex.Substring(1) |> Color.fromHex
    let getColor () =
        let value = reader?Color
        if value <> null then toColor value |> Some
        else None
    while reader.Read() do
        match reader.NodeType, reader.Name with
        | XmlNodeType.Element, "Style" ->
            id <- reader?ID
            background <- None
            borders <- []
            font <- FontStyle.Default
        | XmlNodeType.EndElement, "Style" ->
            let thickness, borderColor = Borders.toThickness borders
            let style =
                {
                  Background = defaultArg background Colors.Transparent
                  Border = {Color = borderColor; Thickness = thickness }
                  Font = font
                }
            styles <- styles.Add(id, style)
        | XmlNodeType.Element, "Borders" ->
            borders <- []
        | XmlNodeType.Element, "Border" ->
            let position = reader?Position
            let weight = reader?Weight |> float
            let color =
                match reader?Color with
                | null -> Colors.Black
                | hex -> hex |> toColor
            borders <- (position,weight,color)::borders
        | XmlNodeType.Element, "Interior" ->
            background <- getColor ()
        | XmlNodeType.Element, "Alignment" ->
            match reader?Horizontal with
            | "Center" -> font <- { font with Alignment = TextAlignment.Center }
            | "Left" -> font <- { font with Alignment = TextAlignment.Left }
            | "Right" -> font <- { font with Alignment = TextAlignment.Right }
            | _ -> ()
            if reader?WrapText <> null then
                font <- { font with Wrap = TextWrapping.Wrap }
        | XmlNodeType.Element, "Font" ->
            match getColor () with
            | Some color -> font <- { font with Foreground = color }
            | None -> ()
            if reader?Bold <> null then font <- { font with Bold = true }
            if reader?Italic <> null then font <- { font with Italic = true }
            match reader?Size with
            | null -> ()
            | size -> font <- { font with Size=float size }
            match reader?Underline with
            | "Single" | " Double" -> font <- { font with Underline=true }
            | null | _ -> ()
            match reader?FontName with
            | null -> ()
            | name -> font <- { font with Family = name }
        | _, _ -> ()
    styles

let readData (cell:Cell) formula (reader:XmlReader) =
    let mutable dataType = ""
    while reader.Read() do
        match reader.NodeType, reader.Name with
        | XmlNodeType.Element, "Data" ->
            dataType <- reader?Type
            let data =
                if formula = null then reader.ReadElementContentAsString()
                else
                try
                    let expr = parse formula
                    formatA1 cell.Address expr
                with _ -> formula
            cell.Data <- data
        | _, _ -> ()

let readTable (sheet:Sheet) (styles:_) (reader:XmlReader) =
    let mutable row = -1
    let mutable col = -1
    while reader.Read() do
        match reader.NodeType, reader.Name with
        | XmlNodeType.Element, "Column" ->
            col <-
                match reader?Index with
                | null -> col + 1
                | n -> (n |> int)-1
            let width = reader?Width |> Double.Parse
            sheet.Columns.[col].Width <- width
        | XmlNodeType.Element, "Row" ->
            row <-
                match reader?Index with
                | null -> row + 1
                | n -> (n |> int) - 1
            col <- 0
        | XmlNodeType.Element, "Cell" ->
            let index = reader?Index
            if index <> null then
                col <- Int32.Parse index - 1
            let cell = sheet.CellAt(ColIndex(col),RowIndex(row))
            let styleID = reader?StyleID
            let style =
                if styleID <> null
                then styles |> Map.tryFind styleID
                else None
            style |> Option.iter (fun (background, border, font) ->
                cell.Background <- background
                cell.BorderStyle <- border
                cell.FontStyle <- font
            )
            let formula = reader?Formula
            reader.ReadSubtree() |> readData cell formula
            col <- col + 1
        | _, _ -> ()

let readAsExcelXml (stream:Stream) =
    use reader = XmlReader.Create(stream)
    let cellStyles =
        if reader.ReadToFollowing("Styles") then
           reader.ReadSubtree() |> readStyles
        else Map.empty
    let styles =
        cellStyles |> Map.map (fun key style ->
            SolidColorBrush style.Background,
                style.Border.ToStyle(),
                    style.Font.ToStyle()
        )
    styles |> Map.map (fun key (_,_,font) -> font) |> Styles.store
    if reader.ReadToFollowing("Table") then
        let colCount = reader?ExpandedColumnCount |> Int32.Parse
        let rowCount = reader?ExpandedRowCount |> Int32.Parse
        let sheet = Sheet(colCount,rowCount,Functions.invoke)
        reader.ReadSubtree() |> readTable sheet styles
        sheet, cellStyles
    else invalidOp "Missing Table definition"