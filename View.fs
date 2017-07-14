namespace Cellz

open System
open System.ComponentModel
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Markup

open System.Windows.Media

type View(sheet:Sheet,load:unit->unit,save:unit->unit) as view =
    inherit UserControl()

    let grid = DataGrid(AutoGenerateColumns=false,
                        HeadersVisibility=DataGridHeadersVisibility.All)

    do  grid.KeyUp.Add (fun e ->
            if e.Key = Key.S && Keyboard.Modifiers = ModifierKeys.Control then
                e.Handled <- true
                save()
            if e.Key = Key.O && Keyboard.Modifiers = ModifierKeys.Control then
                e.Handled <- true
                load()
        )
    do  grid.LoadingRow.Add (fun e ->
            let row = e.Row.DataContext :?> Row
            e.Row.Header <- row.Index
        )
    do  view.Content <- grid

    let createColumn i =
        let header = colIndex.toColName i
        let col = DataGridTemplateColumn(Header=header,
                                         IsReadOnly=false,
                                         Width=DataGridLength(sheet.Columns.[i].Width))
        let path = sprintf "Cells.[%d]" i
        col.ClipboardContentBinding <- Binding(sprintf "%s.Value" path)

#if SILVERLIGHT
        let parse = XamlReader.Load
#else
        let parse = XamlReader.Parse
#endif

        let toDataTemplate s =
            let ns = "http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            sprintf "<DataTemplate xmlns='%s'>%s</DataTemplate>" ns s
            |> parse :?> DataTemplate

        col.CellTemplate <-
            sprintf "<Grid Background='{Binding %s.Background}'>
                      <Border Style='{Binding %s.BorderStyle}'>
                       <TextBlock Style='{Binding %s.FontStyle}'
                                  Text='{Binding %s.Value}'
                                  TextTrimming='WordEllipsis'
                                  ToolTipService.ToolTip='{Binding %s.Formula}'/>
                     </Border>
                     </Grid>" path path path path path
            |> toDataTemplate
        col.CellEditingTemplate <-
            sprintf "<TextBox Text='{Binding %s.Data,Mode=TwoWay}'/>" path
            |> toDataTemplate
        col
    do  for i = 0 to sheet.ColumnCount-1 do createColumn i |> grid.Columns.Add
    do  grid.ItemsSource <- sheet.Rows
    let showError message =
        MessageWindow.show(message, "Error") |> ignore
    do  sheet.Error.Add showError
