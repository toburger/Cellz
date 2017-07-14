namespace Cellz.Tests

open System.Windows
open Microsoft.Silverlight.Testing
open TickSpec

type App() as this = 
    inherit Application()
    let run _ =
        let features = Assembly.FindFeatures typeof<App>.Assembly
        let settings = UnitTestSettings.Make features
        this.RootVisual <- UnitTestSystem.CreateTestPage settings
    do  this.Startup.Add run
