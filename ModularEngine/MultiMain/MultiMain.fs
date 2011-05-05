namespace Microsoft.Research.Dkal.SimpleRouter

open System
open System.IO
open System.Text
open System.Threading
open System.Text.RegularExpressions

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Factories
open Microsoft.Research.Dkal.Router.Factories
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Factories.Initializer
open Microsoft.Research.Dkal.Utils.Exceptions

module MultiMain =

  let createExec(router: IRouter, assembly: Assembly) =
    let kind = "simple"
    let engine = LogicEngineFactory.Engine kind
    let executor = ExecutorFactory.Executor (kind, router, engine, false)

    if assembly.Signature.Substrates.Length > 0 then // TODO: forbid substrates
      printfn "%s: Substrate declarations is fobidden" router.Me
      
    for rule in assembly.Policy.Rules do
      executor.InstallRule rule |> ignore
    executor

  let rec splitPolicies (s : string) =
    let mutable i = 0
    while s.[i]='-' do
      i <- i + 1
    let ppalName = StringBuilder()
    while s.[i]<>'-' do
      ppalName.Append(s.[i]) |> ignore
      i <- i + 1
    while s.[i]='-' do
      i <- i + 1
    let s = s.Substring(i)
    let j = s.IndexOf("---")
    if j<0 then
      [(ppalName.ToString(), s)]
    else
      (ppalName.ToString(), s.Substring(0, j)) :: splitPolicies(s.Substring(j))

  let execute (policy: string) =
    // Populate factories
    FactoriesInitializer.Init()

    let i = policy.IndexOf("---")
    let commonPolicy = policy.Substring(0, i) + "\n"
    let ppals = splitPolicies( policy.Substring(i) )
    let routers = RouterFactory.ManyRouters "local" (ppals |> List.unzip |> fst)        
    let assemblies = ppals |> List.map (fun x -> 
      let parser = ParserFactory.InfonParser("simple", routers.[fst x].Me)
      (fst x, parser.ParseAssembly (commonPolicy + snd x))
    )
    let executors = assemblies |> List.map (fun x ->
      createExec(routers.[fst x], snd x)
    )
    executors |> List.iter (fun x -> x.Start())
    Thread.Sleep(2000)
    executors |> List.iter (fun x -> x.Stop())
    ()

  let args = System.Environment.GetCommandLineArgs() |> Seq.toList
  try  
    match args with
    | [_; policyFile] ->
      if not (File.Exists (policyFile)) then
        printfn "File not found: %O" policyFile
      else
        execute(File.ReadAllText(policyFile))
    | _ -> failwith "Wrong number of parameters"
  with 
  | ParseException(msg, text, line, col) -> printfn "Error while parsing in line %O, column %O: %O\r\n %O" line col msg text
  | e -> printfn "%O" e