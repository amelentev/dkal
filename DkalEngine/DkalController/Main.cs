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

using System;
using System.Collections.Generic;
using System.Windows.Forms;

using E = Microsoft.Research.DkalEngine;
using System.Threading;

namespace Microsoft.Research.DkalController
{
  static class MainClass
  {
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

      int argp = 0;
      int trace = 0;
      string filename = null;
      while (argp < args.Length && args[argp] == "-v") { trace++; argp++; }

      if (argp < args.Length) {
        filename = args[argp++];
      }

      if (filename == null) {
        OpenFileDialog fdlg = new OpenFileDialog();
        if (fdlg.ShowDialog() == DialogResult.OK)
        {
          filename = fdlg.FileName;
        }
        else
        {
          MessageBox.Show("you need to provide a filename (either on the command line or via the dialog)");
          return;
        }
      }

      var decls = new List<E.Ast.Assertion>();
      var pctx = new E.ParsingCtx();
      string me = null;

      try {
        foreach (var a in pctx.ParsePrelude())
          decls.Add(a);
        foreach (var a in pctx.ParseFile(filename)) {
          decls.Add(a);
          me = a.AssertionInfo.principal.name;
        }
      } catch (E.Util.SyntaxError se) {
        MessageBox.Show(se.Data0 + ": " + se.Data1, "Syntax Error");
        return;
      }

      var key = "private_sql" + me;
      if (me == null || !pctx.Options.ContainsKey(key))
        key = "private_sql";

      var opts = E.Options.Create();
      opts.PrivateSql = pctx.Options[key];
      opts.Trace = trace;


      E.Engine e = E.Engine.Config(opts);

      e.Reset();
      foreach (var a in decls)
        e.AddAssertion(a);
      e.AddDefaultFilter();
      System.Threading.Thread.Sleep(500);

      var hooks = new ViewHooks(pctx, e);
      var comm = new CommunicationWindow(filename, hooks, e, pctx);

      Application.Run(comm);

      e.Close();
      } catch (Exception ex)
      {
          MessageBox.Show(ex.ToString());
      }
    }
  }
}
