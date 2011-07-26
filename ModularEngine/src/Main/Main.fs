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
open Microsoft.Research.Dkal.Utils.ErrorCodes

/// Console front-end for a single principal
module Main =

  let private log = LogManager.GetLogger("Main")

  let private args = System.Environment.GetCommandLineArgs() |> Seq.toList
  match args with
  | [_; routingFile; policyFile; step] | [_; routingFile; policyFile; step; "-MLLogicEngine"] | [_; routingFile; policyFile; step; "-FStarLogicEngine"]  ->
      try  
          let kind = "simple"

          if not (File.Exists (routingFile)) then
            log.Fatal("File not found: {0}", routingFile)
          elif not (File.Exists (policyFile)) then
            log.Fatal("File not found: {0}", policyFile)
          else
            // Populate factories
            FactoriesInitializer.Init()

            let router = RouterFactory.Router kind routingFile
            let parser, printer = ParserFactory.InfonParser(kind, router.Me), PrettyPrinterFactory.InfonPrinter kind
            let logicEngineKind = 
              (if List.exists (fun a -> a="-MLLogicEngine") args then "ML" else 
                (if List.exists (fun a -> a="-FStarLogicEngine") args then "FStar" else "simple"))
            let logicEngine = LogicEngineFactory.LogicEngine logicEngineKind
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
      with 
      | ParseException(msg, text, line, col) -> 
        log.Error("{0}({1},{2}): error {3}: {4}", policyFile, line, col, errorParsing, msg)
      | e -> 
        log.ErrorException("Something went wrong", e)
    | _ -> log.Fatal("Wrong number of parameters, expecting: routing file, policy file, step\r\nWhere step must be one of 'step' or 'noStep'")

