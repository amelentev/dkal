namespace Microsoft.Research.Dkal

open NLog
open System
open System.IO
open System.Diagnostics
open System.Text
open System.Threading
open System.Text.RegularExpressions

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Infostrate.Factories
open Microsoft.Research.Dkal.Executor.Factories
open Microsoft.Research.Dkal.LogicEngine.Factories
open Microsoft.Research.Dkal.SignatureProvider.Factories
open Microsoft.Research.Dkal.Router.Factories
open Microsoft.Research.Dkal.Router.Local
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Factories
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Factories.Initializer
open Microsoft.Research.Dkal.Utils.Exceptions

module MultiMain =

  let log = LogManager.GetLogger("MultiMain")

  let messagesLimitExceeded = new AutoResetEvent(false)

  let createExec(router: IRouter, assembly: Assembly) =
    let kind = "simple"
    let infostrate = InfostrateFactory.Infostrate kind
    let logicEngine = LogicEngineFactory.LogicEngine kind 
    let signatureProvider = SignatureProviderFactory.SignatureProvider kind 
    let executor = ExecutorFactory.Executor (kind, router, logicEngine, signatureProvider, infostrate, false)

//    if assembly.Signature.Substrates.Length > 0 then // TODO: forbid substrates
//      printfn "%s: Substrate declarations are fobidden" router.Me
      
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
      try
        (fst x, parser.ParseAssembly (commonPolicy + snd x))
      with ParseException(msg, text, line, col) -> 
        log.Error("Error while parsing in line {0}, column {1}: {2}\r\n {3}", line, col, msg, text)
        Environment.Exit(1); failwith ""
    )
    let fixedPointCounter = new CountdownEvent(assemblies.Length)
    let checkGlobalFixedPoint _ = 
      let reachedGlobalFixedPoint = try fixedPointCounter.Signal() with e -> true
      if reachedGlobalFixedPoint then
        messagesLimitExceeded.Set() |> ignore
    let executors = assemblies |> List.mapi (fun i x ->
      let exec = createExec(routers.[fst x], snd x)
      exec.FixedPointCallback checkGlobalFixedPoint
      exec.WakeUpCallback 
        (fun _ -> 
          fixedPointCounter.TryAddCount() |> ignore)
      if i = 0 then
        (routers.[fst x] :?> LocalRouter).AddMailerCallback msgsLimit 
          (fun _ -> messagesLimitExceeded.Set() |> ignore)
      exec
    )
    executors |> List.iter (fun x -> x.Start())
    let reachedTimeLimit = not <| messagesLimitExceeded.WaitOne(timeLimit) 
    executors |> List.iter (fun x -> x.Stop())
    if reachedTimeLimit then
      log.Info("Time limit exceeded at {0} milliseconds", timeLimit)
    elif fixedPointCounter.IsSet then
      log.Info("Fixed-point reached")
    else
      log.Info("Message limit exceeded at {0} messages", msgsLimit)
    // Do a hard kill (in case the last round takes too long after stop was signaled)
    Thread.Sleep(500)
    Process.GetCurrentProcess().Kill()

  let args = System.Environment.GetCommandLineArgs() |> Seq.toList
  match args with
  | [_; policyFile; timeLimit; msgsLimit] ->
    if not (File.Exists (policyFile)) then
      log.Fatal("File not found: {0}", policyFile)
    else
      try
        execute(File.ReadAllText(policyFile), Int32.Parse(timeLimit), Int32.Parse(msgsLimit))
      with
        e -> log.Error(e.Message)
  | _ -> log.Error("Wrong number of parameters; expecting policy file, time limit (ms), messages limit")
