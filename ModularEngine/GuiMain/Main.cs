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

using NLog;
using System;
using System.IO;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Threading;

using Microsoft.Research.Dkal.Interfaces;
using Microsoft.Research.Dkal.Ast;
using Microsoft.Research.Dkal.Ast.Infon;
using Microsoft.Research.Dkal.Ast.Infon.Syntax.Factories;
using Microsoft.Research.Dkal.Factories.Initializer;
using Microsoft.Research.Dkal.Executor.Factories;
using Microsoft.Research.Dkal.Infostrate.Factories;
using Microsoft.Research.Dkal.LogicEngine.Factories;
using Microsoft.Research.Dkal.Router.Factories;
using Microsoft.Research.Dkal.SignatureProvider.Factories;
using Microsoft.Research.Dkal.Utils.Exceptions;

namespace Microsoft.Research.Dkal.GuiMain
{
    static class MainClass
    {

        static Logger log = LogManager.GetLogger("GuiMain");

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main(string[] args)
        {
            try
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);

                string routingFile, policyFile;

                if (args.Length == 2) {
                    routingFile = args[0];
                    policyFile = args[1];
                } else {
                    MessageBox.Show("Incorrect number of parameters. Expecting: routing table file, policy file");
                    return;
                }
                    
                string kind = "simple";

                // Populate factories
                FactoriesInitializer.Init();

                var router = RouterFactory.Router(kind, routingFile);
                var parser= ParserFactory.InfonParser(kind, router.Me);
                var printer = PrettyPrinterFactory.InfonPrinter(kind);
                var logicEngine = LogicEngineFactory.LogicEngine(kind);
                var signatureProvider = SignatureProviderFactory.SignatureProvider(kind);
                var infostrate = InfostrateFactory.Infostrate(kind);
                var executor = ExecutorFactory.Executor(kind, router, logicEngine, signatureProvider, infostrate);

                Assembly assembly;
                try {
                    assembly = parser.ParseAssembly(File.ReadAllText(policyFile));
                } catch (ParseException e) {
                    MessageBox.Show("Error while parsing in line " + e.Data2 + ", column " + e.Data3 + ": " + e.Data2, "Syntax Error");
                    return;
                }

                log.Info("Principal {0} running...", router.Me);
                log.Debug("------------------------------------------------------------------------");
                log.Debug(printer.PrintPolicy(assembly.Policy));
                log.Debug("------------------------------------------------------------------------");
                foreach (var rule in assembly.Policy.Rules)
                  executor.InstallRule(rule);

                var comm = new CommunicationWindow(policyFile, executor, router, logicEngine, printer, parser);

                Application.Run(comm);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString());
            }
        }
    }
}
