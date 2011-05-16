namespace Microsoft.Research.Dkal

open System.IO

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Factories
open Microsoft.Research.Dkal.Router.Factories
open Microsoft.Research.Dkal.LogicEngine.Factories
open Microsoft.Research.Dkal.SignatureProvider.Factories
open Microsoft.Research.Dkal.Executor.Factories
open Microsoft.Research.Dkal.Infostrate.Factories
open Microsoft.Research.Dkal.Factories.Initializer
open Microsoft.Research.Dkal.Utils.Exceptions

module Main =

  let args = System.Environment.GetCommandLineArgs() |> Seq.toList
  try  
    match args with
    | [_; routingFile; policyFile; kind] ->

      if not (File.Exists (routingFile)) then
        printfn "File not found: %O" routingFile
      elif not (File.Exists (policyFile)) then
        printfn "File not found: %O" policyFile
      else
        // Populate factories
        FactoriesInitializer.Init()

        let router = RouterFactory.Router kind routingFile
        let parser, printer = ParserFactory.InfonParser(kind, router.Me), PrettyPrinterFactory.InfonPrinter kind
        let logicEngine = LogicEngineFactory.LogicEngine kind 
        let signatureProvider = SignatureProviderFactory.SignatureProvider kind 
        let infostrate = InfostrateFactory.Infostrate kind
        let executor = ExecutorFactory.Executor (kind, router, logicEngine, signatureProvider, infostrate)

        let assembly = parser.ParseAssembly (File.ReadAllText policyFile)
        printfn "Principal %O running..." router.Me
        printfn "------------------------------------------------------------------------"
        printfn "%O" <| printer.PrintPolicy assembly.Policy
        printfn "------------------------------------------------------------------------"
        for rule in assembly.Policy.Rules do
          executor.InstallRule rule |> ignore
        executor.Start()

    | _ -> failwith "Wrong number of parameters"
  with 
  | ParseException(msg, text, line, col) -> printfn "Error while parsing in line %O, column %O: %O\r\n %O" line col msg text
  | e -> printfn "%O" e





