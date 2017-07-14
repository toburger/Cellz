using Cellz;
using System;
using Windows.System;
using Windows.UI;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Navigation;
using WinRTXamlToolkit.Controls;

// The Blank Page item template is documented at http://go.microsoft.com/fwlink/?LinkId=234238

namespace Cellz.App
{
    /// <summary>
    /// An empty page that can be used on its own or navigated to within a Frame.
    /// </summary>
    public sealed partial class MainPage : Page
    {
        public MainPage()
    {
            this.InitializeComponent();
            var rows = 5;
            var cols = 5;
            var sheet = new Sheet(cols, rows);
            for(var row = 0; row < rows; ++row)
            {
                for(var col = 0; col < cols; ++col)
                {
                    var data = new TextBox 
                    {  
                        FontSize=64,
                        HorizontalAlignment=HorizontalAlignment.Center, 
                        VerticalAlignment=VerticalAlignment.Center,
                        Visibility=Visibility.Collapsed
                    };
                    var cell = sheet.Rows[row].Cells[col];
                    cell.Data = "=1+1";
                    data.SetBinding(
                        TextBox.TextProperty, 
                        new Binding 
                        { 
                            Source = cell, 
                            Path=new PropertyPath("Data"), 
                            Mode = BindingMode.TwoWay 
                        });                    
                    var value = new TextBlock
                    {
                        FontSize = 64,
                        HorizontalAlignment = HorizontalAlignment.Center,
                        VerticalAlignment = VerticalAlignment.Center
                    };
                    value.SetBinding(
                        TextBlock.TextProperty,
                        new Binding { Source = cell, Path=new PropertyPath("Value") });
                    var panel = new Grid();
                    panel.Children.Add(data);
                    panel.Children.Add(value);
                    Grid.SetColumn(panel, col+1);
                    Grid.SetRow(panel, row+1);
                    Grid.Children.Add(panel);

                    Action HideEditor = () =>
                        {
                            data.Visibility = Visibility.Collapsed;
                            value.Visibility = Visibility.Visible;
                        };

                    data.LostFocus += (s, e) =>
                    {
                        HideEditor();
                    };
                    data.KeyUp += (s, e) =>
                    {
                        if (e.Key == VirtualKey.Escape)
                        {
                            HideEditor();
                            e.Handled = true;
                        }
                        if (e.Key == VirtualKey.Enter)
                        {
                            HideEditor();
                            e.Handled = true;
                        }
                    };
                    value.PointerPressed += (s, e) =>
                    {
                        data.Visibility = Visibility.Visible;
                        value.Visibility = Visibility.Collapsed;
                        data.Focus(FocusState.Programmatic);
                    };
                }
            }
        }

        /// <summary>
        /// Invoked when this page is about to be displayed in a Frame.
        /// </summary>
        /// <param name="e">Event data that describes how this page was reached.  The Parameter
        /// property is typically used to configure the page.</param>
        protected override void OnNavigatedTo(NavigationEventArgs e)
        {
        }
    }
}
