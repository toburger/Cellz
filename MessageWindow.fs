module Cellz.MessageWindow

open System.Windows.Controls

#if SILVERLIGHT
let show (message,caption) =
    let window = ChildWindow(Title=caption, HasCloseButton=false)
    let button = Button(Content="OK")
    async {
        let! result = Async.AwaitEvent button.Click
        window.DialogResult <- System.Nullable(true)
    }   |> Async.StartImmediate
    let (+) (panel:Panel) child = panel.Children.Add child; panel
    window.Content <-
        StackPanel(Orientation=Orientation.Vertical) +
            TextBlock(Text=message) +
            button
    window.Show()
#else
let show (message:string,caption:string) =
    System.Windows.MessageBox.Show(message, caption) |> ignore
#endif
