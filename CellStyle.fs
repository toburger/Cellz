namespace Cellz

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Media

module Styles =
    let resources = Application.Current.Resources
    let store styles =
        styles |> Map.iter (fun key style ->
            if resources.Contains(key) then resources.Remove(key)
            resources.Add(key,style)
        )
    let tryFind (style:Style) =
#if SILVERLIGHT
        resources
        |> Seq.tryFind (fun pair -> pair.Value = box style)
        |> Option.map (fun pair -> string pair.Key)
#else
        resources.Keys
        |> Seq.cast
        |> Seq.tryFind (fun key -> resources.Item(key) = box style)
        |> Option.map string
#endif

module Color =
    open System.Globalization
    let fromHex s =
        let n = Int32.Parse(s, NumberStyles.HexNumber)
        let r = (n >>> 16) &&& 0xff
        let g = (n >>> 8) &&& 0xff
        let b = (n >>> 0) &&& 0xff
        Color.FromArgb(0xffuy,byte r,byte g,byte b)
    let toHex (color:Color) =
        let n =
            (int color.R <<< 16) |||
            (int color.G <<< 8) |||
            (int color.B)
        "#" + n.ToString("X06")

[<AutoOpen>]
module Converters =
    let valueConverter (f:'TValue -> 'TResult) =
        { new IValueConverter with
                member this.Convert(value, targetType, parameter, culture) =
                    f (value :?> 'TValue) |> box
                member this.ConvertBack(value, targetType, parameter, culture) =
                    new NotImplementedException() |> raise
        }

type CellStyle = {
    Background: Color
    Border : BorderStyle
    Font: FontStyle
    } with
    static member Default = {
        Background = Colors.Transparent
        Border = BorderStyle.Default
        Font = FontStyle.Default
        }

and BorderStyle = { Color:Color; Thickness:Thickness } with
    static member Default = {
        Color=Colors.Transparent;
        Thickness=Thickness()
        }
    member source.ToStyle() =
        let borderStyle = Style(TargetType = typeof<System.Windows.Controls.Border>)
        let color = Binding("Color", Source=source, Mode=BindingMode.OneWay)
        color.Converter <- valueConverter (fun color ->  SolidColorBrush color)
        borderStyle.Setters.Add(Setter(Border.BorderBrushProperty, color))
        let thickness = Binding("Thickness", Source=source, Mode=BindingMode.OneWay)
        borderStyle.Setters.Add(Setter(Border.BorderThicknessProperty, thickness))
        borderStyle

and FontStyle = {
    Foreground:Color
    Size:float
    Family:string
    Bold:bool
    Italic:bool
    Underline:bool
    Alignment:TextAlignment
    Wrap:TextWrapping
    } with
    static member Default = {
        Foreground = Colors.Black
        Size = 11.0
        Family = null
        Bold = false
        Italic = false
        Underline = false
        Alignment = TextAlignment.Left
        Wrap = TextWrapping.NoWrap
        }
    member source.ToStyle() =
        let style = Style()
        style.TargetType <- typeof<TextBlock>
        let add = style.Setters.Add
        let foreground = SolidColorBrush source.Foreground
        Setter(TextBlock.ForegroundProperty, foreground) |> add
        let bold = if source.Bold then FontWeights.Bold else FontWeights.Normal
        Setter(TextBlock.FontWeightProperty, bold) |> add
        let italic = if source.Italic then FontStyles.Italic else FontStyles.Normal
        Setter(TextBlock.FontStyleProperty, italic) |> add
        let underline = if source.Underline then TextDecorations.Underline else null
        Setter(TextBlock.TextDecorationsProperty, underline) |> add
        let size = source.Size
        Setter(TextBlock.FontSizeProperty, size) |> add
#if SILVERLIGHT
        let family = source.Family
#else
        let family = FontFamily(source.Family)
#endif
        Setter(TextBlock.FontFamilyProperty, family) |> add
        let alignment = source.Alignment
        Setter(TextBlock.TextAlignmentProperty, alignment) |> add
        let wrap = source.Wrap
        Setter(TextBlock.TextWrappingProperty, wrap) |> add
        style