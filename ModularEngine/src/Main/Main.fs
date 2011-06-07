// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal

open System.IO
open NLog

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Factories
open Microsoft.Research.Dkal.Router.Factories
open Microsoft.Research.Dkal.LogicEngine.Factories
open Microsoft.Research.Dkal.MailBox.Factories
open Microsoft.Research.Dkal.SignatureProvider.Factories
open Microsoft.Research.Dkal.Executor.Factories
open Microsoft.Research.Dkal.Infostrate.Factories
open Microsoft.Research.Dkal.Factories.Initializer
open Microsoft.Research.Dkal.Utils.Exceptions

module Main =

  let log = LogManager.GetLogger("Main")

  let args = System.Environment.GetCommandLineArgs() |> Seq.toList
  try  
    match args with
    | [_; routingFile; policyFile; step] ->

      let kind = "simple"

      if not (File.Exists (routingFile)) then
        log.Error("File not found: {0}", routingFile)
      elif not (File.Exists (policyFile)) then
        log.Error("File not found: {0}", policyFile)
      else
        // Populate factories
        FactoriesInitializer.Init()

        let router = RouterFactory.Router kind routingFile
        let parser, printer = ParserFactory.InfonParser(kind, router.Me), PrettyPrinterFactory.InfonPrinter kind
        let logicEngine = LogicEngineFactory.LogicEngine kind 
        let signatureProvider = SignatureProviderFactory.SignatureProvider kind 
        let infostrate = InfostrateFactory.Infostrate kind
        let mailbox = MailBoxFactory.MailBox kind logicEngine
        let executor = ExecutorFactory.Executor (kind, router, logicEngine, signatureProvider, infostrate, mailbox)

        // suscribe callbacks to executor
        if step = "step" then
          executor.RoundStartCallback (fun _ -> System.Console.ReadKey() |> ignore)
        elif step <> "noStep" then
          failwithf "Step parameter must be one of 'step' or 'noStep', found %O" step

        let assembly = parser.ParseAssembly (File.ReadAllText policyFile)
        log.Info("Principal {0} running...", router.Me)
        log.Debug("------------------------------------------------------------------------")
        log.Debug(printer.PrintPolicy assembly.Policy)
        log.Debug("------------------------------------------------------------------------")
        for rule in assembly.Policy.Rules do
          executor.InstallRule rule |> ignore
        executor.Start()

    | _ -> log.Error("Wrong number of parameters, expecting: routing file, policy file, step\r\nWhere step must be one of 'step' or 'noStep'")
  with 
  | ParseException(msg, text, line, col) -> 
    log.Error("Error while parsing in line {0}, column {1}: {2}\r\n {3}", line, col, msg, text)
  | e -> 
    log.ErrorException("Something went wrong", e)
    

