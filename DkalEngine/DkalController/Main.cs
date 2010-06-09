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
        MessageBox.Show("expecting filename on command line");
        return;
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
    }
  }
}

