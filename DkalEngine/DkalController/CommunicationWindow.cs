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
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using E = Microsoft.Research.DkalEngine;


namespace Microsoft.Research.DkalController
{

  public partial class CommunicationWindow : Form
  {
    E.Engine eng;
    E.ParsingCtx pctx;
    ViewHooks hooks;

    Dictionary<string, SetStyle> styles = new Dictionary<string, SetStyle>();

    private void AddStyles()
    {
      styles["b"] = () => { richTextBox1.SelectionFont = new Font(richTextBox1.Font, FontStyle.Bold); richTextBox1.SelectionColor = Color.Red; };
      styles["red"] = () => { richTextBox1.SelectionColor = Color.Red; };
      styles["blue"] = () => { richTextBox1.SelectionColor = Color.Blue; };
    }

    int GetInt(string name)
    {
      string s;
      int i;
      if (pctx.Options.TryGetValue(name, out s) && int.TryParse(s, out i)) return i;
      return -1;
    }

    void InitialMessage(string f)
    {
      Say(string.Format(
        "<b>LOADED DKAL FILE</b> <blue>{0}</blue> ({1} infons, {2} comm.rules, {3} filters)\n", f, eng.infostrate.Length, eng.communications.Length, eng.filters.Length)); 

      string inp = null;
      if (pctx.Options.TryGetValue("initial_input", out inp)) {
        textBox1.Text = inp;
        textBox1.SelectAll();
      }

      Text += ": " + pctx.Me.name;

      SetPosition(GetInt("x"), GetInt("y"), GetInt("w"), GetInt("h"));
    }

    public CommunicationWindow(string fileName, ViewHooks h, E.Engine eng, E.ParsingCtx pctx)
    {
      h.comm = this;
      InitializeComponent();
      AddStyles();
      this.pctx = pctx;
      this.eng = eng;
      this.hooks = h;
      var sink = new LogSink(richTextBox2);
      Console.SetOut(sink);
      Console.SetError(sink);
      Console.WriteLine("Hello.");
      richTextBox3.Text = System.IO.File.ReadAllText(fileName);
      InitialMessage(fileName);
    }

    public void SetPosition(int x, int y, int w, int h)
    {
      if (w > 50)
        this.Width = w;
      if (h > 50)
        this.Height = h;
      if (x >= 0)
        this.Left = x;
      if (y >= 0)
        this.Top = y;
    }

    public void Say(string msg)
    {
      int i = 0;
      StringBuilder sb = new StringBuilder();
      var begs = new Dictionary<string, int>();
      while (i < msg.Length) {
        if (msg[i] == '<') {
          int j = i + 1;
          while (j < msg.Length) {
            if (msg[j] == '>') break;
            j++;
          }
          if (j < msg.Length) {
            var s = msg.Substring(i + 1, j - i - 1);

            var ending = false;
            if (s.StartsWith("/")) {
              s = s.Substring(1);
              ending = true;
            }

            if (styles.ContainsKey(s)) {
              richTextBox1.AppendText(sb.ToString());
              sb.Length = 0;

              if (ending) {
                int start = begs[s];
                richTextBox1.SelectionStart = start;
                richTextBox1.SelectionLength = richTextBox1.Text.Length - start;
                styles[s]();
                richTextBox1.SelectionStart = richTextBox1.Text.Length;
                richTextBox1.SelectionLength = 0;
                richTextBox1.SelectionFont = new Font(richTextBox1.Font, FontStyle.Regular);
                richTextBox1.SelectionColor = Color.Black;
              } else {
                begs[s] = richTextBox1.Text.Length;
              }

              i = j + 1;
              continue;
            }
          }

        }

        sb.Append(msg[i++]);
      }
      richTextBox1.AppendText(sb.ToString());
      richTextBox1.ScrollToCaret();
    }

    private void richTextBox1_TextChanged(object sender, EventArgs e)
    {

    }

    private void button1_Click(object sender, EventArgs e)
    {

    }

    private void button3_Click(object sender, EventArgs e)
    {
    }

    private void button4_Click(object sender, EventArgs e)
    {
    }

    private void button3_Click_1(object sender, EventArgs e)
    {
    }

    private void button4_Click_1(object sender, EventArgs e)
    {
    }

    private void splitContainer1_Panel1_Paint(object sender, PaintEventArgs e)
    {

    }

    private void CommunicationWindow_Load(object sender, EventArgs e)
    {

    }

    private void button1_Click_1(object sender, EventArgs e)
    {
      if (textBox1.Text == "ALL") {
          foreach (var i in eng.infostrate)
          this.Say(string.Format("<b>INFON:</b>\n{0}\n", i.infon));
        return;
      }

      try {
        eng.Ask(hooks, pctx.ParseInfon(textBox1.Text));
      } catch (Exception ex) { hooks.ExceptionHandler(ex); }
      textBox1.SelectAll();
      textBox1.Focus();
    }

    private void button2_Click(object sender, EventArgs e)
    {
      try {
        eng.AddInfon(pctx.ParseInfon(textBox1.Text));
      } catch (Exception ex) { hooks.ExceptionHandler(ex); }
      textBox1.SelectAll();
      textBox1.Focus();
    }

    private void button3_Click_2(object sender, EventArgs e)
    {
      hooks.Step();
    }
  }

  delegate void DoAction();

  public class LogSink : System.IO.StringWriter
  {
    const int SizeLimit = 10000;
    int pos = 0;
    RichTextBox log;

    public LogSink(RichTextBox l)
    {
      log = l;
    }

    public void Update()
    {
      DoAction upd = null;

      var sb = this.GetStringBuilder();
      if (sb.Length > SizeLimit) {
        sb.Remove(0, sb.Length - (SizeLimit / 2));
        var curVal = sb.ToString();
        upd = () => log.Text = curVal;
      } else {
        int len = sb.Length - pos;
        if (len > 0) {
          char[] buf = new char[len];
          sb.CopyTo(pos, buf, 0, len);
          var appVal = new String(buf);
          upd = () => log.AppendText(appVal);
        }
      }
      try {
        log.BeginInvoke(upd);
        pos = sb.Length;
      } catch (System.Exception) {
      }
    }

    public override void Write(char value)
    {
      base.Write(value);
      this.Update();
    }

    public override void Write(char[] buffer, int index, int count)
    {
      base.Write(buffer, index, count);
      this.Update();
    }

    public override void Write(string value)
    {
      base.Write(value);
      this.Update();
    }
  }


  delegate void SetStyle();

}
