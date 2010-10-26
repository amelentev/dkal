namespace Microsoft.Research.GeneralPDP

open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.GeneralPDP.Scenario.ScenarioViewer
open Microsoft.Research.GeneralPDP.Scenario.Basics
open Microsoft.Research.GeneralPDP.Scenario.Parsing

open System
open System.IO
open System.Threading

module Main =
  let main () =
    let args = System.Environment.GetCommandLineArgs() |> Seq.toList |> List.tail
    match args with 
    | [scenarioFile] ->
      try
        let rawContents = File.ReadAllText(scenarioFile)
        let scenario, steps = parseScenario rawContents
  
        let scenarioViewer = ScenarioViewer()
        scenario.SuscribeViewer(scenarioViewer)
        scenarioViewer.Display()
        scenario.Start()

        for step in steps do
          match step with
          | MessageStep(message) -> scenario.Route(message)
          | SleepStep(amount) -> Thread.Sleep(amount)

        scenarioViewer.WaitForWindow()
        scenario.UnsuscribeViewer(scenarioViewer)
        scenario.CheckPoint()
        scenario.Finish()

      with 
      | SyntaxError(p,s) -> printfn "%O: %O" p s
      | ex -> printfn "Unhandled Exception: %O" ex
    | _ -> printfn "Expecting an argument (Scenario XML file)"

  do main()


