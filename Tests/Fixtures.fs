namespace TickSpec.NUnit

open NUnit.Framework
open System.IO
open System.Reflection
open TickSpec

/// Inherit from FeatureFixture to define a feature fixture
[<AbstractClass;TestFixture>]
type FeatureFixture (source:string) =
    let assembly = typeof<FeatureFixture>.Assembly
    let definitions = new StepDefinitions(assembly)
    [<TestCaseSource("Scenarios")>]
    member this.TestScenario (scenario:Scenario) =
        if scenario.Tags |> Seq.exists ((=) "ignore") then
            raise (new IgnoreException("Ignored: " + scenario.Name))
        scenario.Action.Invoke()
    member this.Scenarios =
        let s = File.OpenText(Path.Combine(@"..\..\",source))
        definitions.GenerateScenarios(source,s)

type ``Literal values`` () = inherit FeatureFixture("Literal values.txt")
type ``Sum cells`` () = inherit FeatureFixture("Sum cells.txt")
type ``Boolean logic`` () = inherit FeatureFixture("Boolean Logic.txt")
type ``Change propagation`` () = inherit FeatureFixture("Change propagation.txt")
type ``Circular reference`` () = inherit FeatureFixture("Circular Reference.txt")