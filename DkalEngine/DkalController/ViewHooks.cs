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
using System.Text;
using E = Microsoft.Research.DkalEngine;

namespace Microsoft.Research.DkalController
{
  public class ViewHooks : E.ICommunicator
  {
    internal CommunicationWindow comm;
    E.ParsingCtx pctx;
    E.Engine eng;
    E.SqlCommunicator sqlcomm;

    private void Say(string msg, params object[] args)
    {
      var s = string.Format(msg, args);
      comm.BeginInvoke((DoAction)(() => comm.Say(s)));
    }

    public ViewHooks(E.ParsingCtx pctx, E.Engine eng)
    {
      this.pctx = pctx;
      this.eng = eng;
      sqlcomm = new E.SqlCommunicator(pctx);
    }


    internal void Step()
    {
      FSharp.Core.FSharpOption<E.Ast.Message> msg = null;

      lock (sqlcomm) {
        msg = sqlcomm.CheckForMessage();
      }

      if (msg != null) {
        eng.Listen(this, msg.Value);
        Recieved(msg.Value);
      } else
        eng.Talk(this);
    }

    void Recieved(E.Ast.Message msg)
    {
      Say("<b>FROM</b> {0} <b>GOT</b>\n{1}\n", msg.source, msg.message.Sanitize());
      if (!msg.proviso.IsEmpty)
        Say("    <blue>PROVIDED</blue>\n{0}\n", msg.proviso);
    }

    #region ICommunicator Members
    public void ExceptionHandler(Exception value)
    {
      E.Util.SyntaxError se = value as E.Util.SyntaxError;
      if (se != null)
        Say("<b>ERROR</b> <blue>{0}</blue>: {1}\n", se.Data0, se.Data1);
      else
        Say("<b>Excaption:</b>\n{0}\n", value);
    }

    public E.Ast.Principal PrincipalById(int value)
    {
      lock(sqlcomm)
        return sqlcomm.PrincipalById(value);
    }

    public int PrincipalId(E.Ast.Principal value)
    {
      lock(sqlcomm)
        return sqlcomm.PrincipalId(value);
    }

    public void SendMessage(E.Ast.Message msg)
    {
      lock(sqlcomm)
        sqlcomm.SendMessage(msg);

      Say("<b>SENT TO</b> {0}\n{1}\n", msg.target, msg.message.Sanitize());
      if (!msg.proviso.IsEmpty)
        Say("    <blue>PROVIDED</blue>\n{0}\n", msg.proviso);
    }

    public void Knows(E.Ast.Knows k)
    {
        //Say(string.Format("<b>KNOWS</b>\n{0}\n", k.infon.Sanitize()));
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

    public void Warning(string s)
    {
      Say("<b>WARNING</b> <blue>{0}</blue>\n", s);
    }

    public void RequestFinished()
    {
    }
    #endregion
  }
}
