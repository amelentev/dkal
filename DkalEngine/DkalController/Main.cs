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

      var comm = new CommunicationWindow();
      var hooks = new ViewHooks(comm);
      E.Engine e = E.Engine.Make(trace, hooks);
      hooks.eng = e;
      comm.eng = e;

      if (filename != null)
        e.AsyncLoad(filename);

      var t = new Thread(() => { Thread.Sleep(100); e.EventLoop(); });
      t.Start();

      Application.Run(comm);

      t.Abort();
    }
  }
}
