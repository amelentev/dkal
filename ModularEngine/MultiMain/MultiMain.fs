namespace Microsoft.Research.Dkal.SimpleRouter

open System
open System.IO
open System.Diagnostics
open System.Text
open System.Threading
open System.Text.RegularExpressions

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Factories
open Microsoft.Research.Dkal.Router.Factories
open Microsoft.Research.Dkal.Router.Local
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Factories.Initializer
open Microsoft.Research.Dkal.Utils.Exceptions

module MultiMain =

  let messagesLimitExceeded = new AutoResetEvent(false)

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

  let execute (policy: string, timeLimit: int, msgsLimit: int) =
    // Populate factories
    FactoriesInitializer.Init()

    let i = policy.IndexOf("---")
    let commonPolicy = policy.Substring(0, i) + "\n"
    let ppals = splitPolicies( policy.Substring(i) )
    let routers = RouterFactory.LocalRouters (ppals |> List.unzip |> fst)        
    let assemblies = ppals |> List.map (fun x -> 
      let parser = ParserFactory.InfonParser("simple", routers.[fst x].Me)
      (fst x, parser.ParseAssembly (commonPolicy + snd x))
    )
    let executors = assemblies |> List.mapi (fun i x ->
      let exec = createExec(routers.[fst x], snd x)
      if i = 0 then
        (routers.[fst x] :?> LocalRouter).AddMailerCallback msgsLimit 
          (fun _ -> messagesLimitExceeded.Set() |> ignore)
      exec
    )
    executors |> List.iter (fun x -> x.Start())
    let reachedTimeLimit = not <| messagesLimitExceeded.WaitOne(timeLimit) 
    executors |> List.iter (fun x -> x.Stop())
    if reachedTimeLimit then
      printfn "Time limit exceeded at %O milliseconds" timeLimit
    else
      printfn "Message limit exceeded at %O messages" msgsLimit 
    // Do a hard kill (in case the last round takes too long after stop was signaled)
    Thread.Sleep(500)
    Process.GetCurrentProcess().Kill()

  let args = System.Environment.GetCommandLineArgs() |> Seq.toList
  try  
    match args with
    | [_; policyFile; timeLimit; msgsLimit] ->
      if not (File.Exists (policyFile)) then
        printfn "File not found: %O" policyFile
      else
        execute(File.ReadAllText(policyFile), Int32.Parse(timeLimit), Int32.Parse(msgsLimit))
    | _ -> failwith "Wrong number of parameters; expecting policy file, time limit (ms), messages limit"
  with 
  | ParseException(msg, text, line, col) -> printfn "Error while parsing in line %O, column %O: %O\r\n %O" line col msg text
  | e -> printfn "%O" e