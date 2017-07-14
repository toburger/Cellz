namespace Cellz

open System.Windows.Controls
#if SILVERLIGHT
#else
open Microsoft.Win32
#endif

type Frame (sheet:Sheet) as frame =
    inherit UserControl()
    let panel = Grid()
    do  frame.Content <- panel
    let save (sheet,styles) () =
        let dlg = SaveFileDialog()
        dlg.Filter <- "Microsoft Excel (*.xml)|*.xml";
        dlg.DefaultExt <- "xml";
        if dlg.ShowDialog().Value then
            try using (dlg.OpenFile()) (writeAsExcelXml sheet styles)
            with e -> MessageWindow.show (e.Message, "Error")
    let rec load () =
        let dlg = OpenFileDialog()
        dlg.Filter <- "Microsoft Excel (*.xml)|*.xml";
        if dlg.ShowDialog().Value then
            try
#if SILVERLIGHT
            let stream = dlg.File.OpenRead()
#else
            let stream = System.IO.File.OpenRead(dlg.FileName)
#endif
            let sheet, cellStyles = using (stream) readAsExcelXml
            panel.Children.Clear()
            View(sheet,load, save(sheet,cellStyles)) |> panel.Children.Add |> ignore
            with e -> MessageWindow.show (e.Message, "Error")
    let view = View(sheet,load,save (sheet,Map.empty))
    do panel.Children.Add view |> ignore
