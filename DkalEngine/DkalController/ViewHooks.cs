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
    bool lastMsgCertified = false;

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
      //if (lastMsgCertified)
      //  Say(string.Format("<b>KNOWS</b>\n{0}\n", k.infon.Sanitize()));
    }

    public void Recieved(E.Ast.Message msg)
    {
      Say("<b>FROM</b> {0} <b>GOT</b>\n{1}\n", msg.source, msg.message.Sanitize());
      if (!msg.proviso.IsEmpty)
        Say("    <blue>PROVIDED</blue>\n{0}\n", msg.proviso);
      lastMsgCertified = msg.IsCertified;
    }

    public void Send(E.Ast.Message msg)
    {
      Say("<b>SENT TO</b> {0}\n{1}\n", msg.target, msg.message.Sanitize());
      if (!msg.proviso.IsEmpty)
        Say("    <blue>PROVIDED</blue>\n{0}\n", msg.proviso);
    }

    public void QueryResults(E.Ast.Term inf, IEnumerable<IEnumerable<E.Binding>> results)
    {
      Say("<b>QUERY</b>\n{0}\n", inf);
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


    int GetInt(string name)
    {
      string s;
      int i;
      if (eng.ctx.options.TryGetValue(name, out s) && int.TryParse(s, out i)) return i;
      return -1;
    }

    public void Loaded(string f)
    {
      Say("<b>LOADED DKAL FILE</b> <blue>{0}</blue> ({1} infons, {2} comm.rules, {3} filters)\n", f, eng.infonstrate.Length, eng.communications.Length, eng.filters.Length);
      var me = eng.me.Value.name;
      string inp = null;
      if (!eng.ctx.options.TryGetValue("initial_input", out inp))
        inp = "";
      comm.BeginInvoke((E.Action)(() => { comm.SetPosition(GetInt("x"), GetInt("y"), GetInt("w"), GetInt("h")); }));
      comm.BeginInvoke((E.Action)(() => { comm.Loaded(me, inp);   } ));
    }

    public void Warning(string s)
    {
      Say("<b>WARNING/b> <blue>{0}</blue>\n", s);
    }
      

    #endregion
  }
}
