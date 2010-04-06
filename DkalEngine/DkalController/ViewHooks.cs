using System;
using System.Collections.Generic;
using System.Text;
using E = Microsoft.Research.DkalEngine;

namespace Microsoft.Research.DkalController
{
  class ViewHooks : E.IViewHooks
  {
    CommunicationWindow comm;
    internal E.Engine eng;

    private void Say(string msg, params object[] args)
    {
      var s = string.Format(msg, args);
      comm.BeginInvoke((DoAction)(() => comm.Say(s)));
    }

    public ViewHooks(CommunicationWindow c)
    {
      comm = c;
    }

    #region IViewHooks Members

    public void Knows(E.Ast.Knows k)
    {
      //Say(string.Format("<b>KNOWS</b> {0}\n", k.infon));
    }

    public void Recieved(E.Ast.Message msg)
    {
      Say("<b>FROM</b> {0} <b>GOT</b> {1}\n", msg.source, msg.message);
      if (!msg.proviso.IsEmpty)
        Say("    <blue>PROVIDED</blue> {0}\n", msg.proviso);
    }

    public void Send(E.Ast.Message msg)
    {
      Say("<b>TO</b> {0} <b>SEND</b> {1}\n", msg.target, msg.message);
      if (!msg.proviso.IsEmpty)
        Say("    <blue>PROVIDED</blue> {0}\n", msg.proviso);
    }

    public void QueryResults(E.Ast.Term inf, IEnumerable<IEnumerable<E.Binding>> results)
    {
      Say("<b>QUERY</b> {0}\n", inf);
      foreach (var result in results) {
        var sb = new StringBuilder();
        sb.AppendFormat("  <blue>RESULT:</blue>\n");
        foreach (var binding in result) {
          sb.AppendFormat("      {0} -> {1}\n", binding.formal.name, binding.actual);
        }
        Say(sb.ToString());
      }
    }
    
    public void SyntaxError(E.Util.Pos pos, string msg)
    {
      Say("<b>ERROR</b> <blue>{0}</blue>: {1}\n", pos, msg);
    }


    public void Loaded(string f)
    {
      Say("<b>LOADED DKAL FILE</b> <blue>{0}</blue> ({1} infons, {2} comm.rules, {3} filters)\n", f, eng.infonstrate.Length, eng.communications.Length, eng.filters.Length);
      var me = eng.me.Value.name;
      string inp = null;
      if (!eng.ctx.options.TryGetValue("initial_input", out inp))
        inp = "";
      comm.BeginInvoke((E.Action)(() => { comm.Loaded(me, inp);   } ));
    }

    #endregion
  }
}
