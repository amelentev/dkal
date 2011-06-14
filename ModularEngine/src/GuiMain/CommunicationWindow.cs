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
using System.Threading;
using System.Windows.Forms;
using Microsoft.FSharp.Core;

using Microsoft.Research.Dkal.Interfaces;
using Microsoft.Research.Dkal.Ast;
using Microsoft.Research.Dkal.Ast.Infon;
using Microsoft.Research.Dkal.Utils.Exceptions;

namespace Microsoft.Research.Dkal.GuiMain
{

    /// <summary>
    /// The Form used as GUI
    /// </summary>
    public partial class CommunicationWindow : Form
    {
        IExecutor executor;
        IRouter router;
        ILogicEngine logicEngine;
        IInfonPrettyPrinter prettyPrinter;
        IInfonParser parser;
        string policyFile;

        AutoResetEvent step = new AutoResetEvent(false);

        /// <summary>
        /// Constructs a CommunicationWindow to run a principal with the given
        /// policy, using the provided executor, router, engine, etc.
        /// </summary>
        public CommunicationWindow(string policyFile, IExecutor executor, IRouter router, 
                                    ILogicEngine logicEngine, IInfonPrettyPrinter prettyPrinter, IInfonParser parser)
        {
            this.policyFile = policyFile;
            InitializeComponent();
            AddStyles();
            this.executor = executor;
            this.router = router;
            this.logicEngine = logicEngine;
            this.prettyPrinter = prettyPrinter;
            this.parser = parser;
            var sink = new LogSink(richTextBox2);
            Console.SetOut(sink);
            Console.SetError(sink);
            richTextBox3.Text = System.IO.File.ReadAllText(policyFile);

            // set executor callbacks
            executor.RoundStartCallback(FuncConvert.ToFSharpFunc(new Converter<Unit, Unit>((Unit u) =>
            {
                step.WaitOne();
                return null;
            })));

            executor.ReceiveCallback(
            FuncConvert.ToFSharpFunc(new Converter<ITerm, FSharpFunc<ITerm, Unit>>((msg) =>
            {
                return FuncConvert.ToFSharpFunc(new Converter<ITerm, Unit>((from) =>
                {
                    string printedFrom = prettyPrinter.PrintTerm(from);
                    string printedMsg = prettyPrinter.PrintTerm(msg);
                    Say(string.Format("<b>FROM</b> {0} <b>GOT</b>\n{1}\n", printedFrom, printedMsg));
                    return null;
                }));
            })));

            executor.ActionCallback(
            FuncConvert.ToFSharpFunc(new Converter<ITerm, Unit>((action) =>
            {
                string printedAction = prettyPrinter.PrintTerm(action);
                Say(string.Format("<b>PERFORMED ACTION</b>\n{0}\n", printedAction));
                return null;
            })));

            // start executor
            executor.Start();

        }

        string Id
        {
            get
            {
                return router.Me;
            }
        }

        Dictionary<string, SetStyle> styles = new Dictionary<string, SetStyle>();

        private void AddStyles()
        {
            styles["b"] = () => { richTextBox1.SelectionFont = new Font(richTextBox1.Font, FontStyle.Bold); richTextBox1.SelectionColor = Color.Red; };
            styles["red"] = () => { richTextBox1.SelectionColor = Color.Red; };
            styles["blue"] = () => { richTextBox1.SelectionColor = Color.Blue; };
        }

        void InitialMessage(string f)
        {
            Say(string.Format("<b>LOADED DKAL FILE</b> <blue>{0}</blue>\n", f));

            Text += ": " + Id;

            RestorePosition();
        }

        /// <summary>
        /// Called upon closing, we save the position of the window on the 
        /// registry
        /// </summary>
        protected override void OnClosing(CancelEventArgs e)
        {
            step.Set();
            executor.Stop();
            base.OnClosing(e);
            SavePosition();
        }

        const string RegistryPath = @"Software\Microsoft Research\DKAL\GuiMain\Position";
        void SavePosition()
        {
            var rect = string.Format("{0} {1} {2} {3}", this.Left, this.Top, this.Width, this.Height);
            var key = Microsoft.Win32.Registry.CurrentUser.CreateSubKey(RegistryPath);
            key.SetValue(Id, rect);
        }

        void RestorePosition()
        {
            var key = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(RegistryPath);
            if (key != null)
            {
                var val = key.GetValue(Id) as string;
                if (val != null)
                {
                    var words = val.Split(' ');
                    if (words.Length == 4)
                    {
                        int tmp;
                        if (int.TryParse(words[0], out tmp)) this.Left = tmp;
                        if (int.TryParse(words[1], out tmp)) this.Top = tmp;
                        if (int.TryParse(words[2], out tmp)) this.Width = tmp;
                        if (int.TryParse(words[3], out tmp)) this.Height = tmp;
                    }
                }
            }
        }


        /// <summary>
        /// Called when the Form is loaded to print a message
        /// </summary>
        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            InitialMessage(policyFile);
        }

        /// <summary>
        /// Set the position of the window to the given values
        /// </summary>
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

        /// <summary>
        /// Write to the main textbox, replacing markup by actual formatting
        /// </summary>
        public void Say(string msg)
        {
            int i = 0;
            StringBuilder sb = new StringBuilder();
            var begs = new Dictionary<string, int>();
            while (i < msg.Length)
            {
                if (msg[i] == '<')
                {
                    int j = i + 1;
                    while (j < msg.Length)
                    {
                        if (msg[j] == '>') break;
                        j++;
                    }
                    if (j < msg.Length)
                    {
                        var s = msg.Substring(i + 1, j - i - 1);

                        var ending = false;
                        if (s.StartsWith("/"))
                        {
                            s = s.Substring(1);
                            ending = true;
                        }

                        if (styles.ContainsKey(s))
                        {
                            richTextBox1.AppendText(sb.ToString());
                            sb.Length = 0;

                            if (ending)
                            {
                                int start = begs[s];
                                richTextBox1.SelectionStart = start;
                                richTextBox1.SelectionLength = richTextBox1.Text.Length - start;
                                styles[s]();
                                richTextBox1.SelectionStart = richTextBox1.Text.Length;
                                richTextBox1.SelectionLength = 0;
                                richTextBox1.SelectionFont = new Font(richTextBox1.Font, FontStyle.Regular);
                                richTextBox1.SelectionColor = Color.Black;
                            }
                            else
                            {
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

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void aboutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            MessageBox.Show("For more info about DKAL please visit http://dkal.codeplex.com", "DKAL v" + Application.ProductVersion);
        }

        private void button1_Click_1(object sender, EventArgs e)
        {
            try
            {
                if (textBox1.Text == "ALL") {
                    Say(string.Format("<b>KNOWLEDGE FOR</b>\n{0}\n", router.Me));
                    foreach (var infon in logicEngine.Infostrate.Knowledge)
                    {
                        Say(string.Format("{0}\n", infon));
                    }
                    Say(string.Format("<b>END OF KNOWLEDGE</b>\n"));
                } else {
                    var infon = parser.ParseInfon(textBox1.Text);
                    Say(string.Format("<b>RESULTS FOR</b>\n{0}\n", infon));
                    foreach (var subst in logicEngine.Derive(infon, new ISubstitution[] {Substitution.Id})) {
                        Say(string.Format("{0}\n", subst));
                    }
                    Say(string.Format("<b>END OF RESULTS</b>\n"));
                }
            }
            catch (ParseException ex)
            {
                MessageBox.Show("Error while parsing in line " + ex.Data2 + ", column " + ex.Data3 + ": " + ex.Data2, "Syntax Error");
                return;
            }
            textBox1.SelectAll();
            textBox1.Focus();
        }

        private void button2_Click(object sender, EventArgs e)
        {
            try
            {
                var infon = parser.ParseInfon(textBox1.Text);
                logicEngine.Infostrate.Learn(infon);
            }
            catch (ParseException ex)
            {
                MessageBox.Show("Error while parsing in line " + ex.Data2 + ", column " + ex.Data3 + ": " + ex.Data2, "Syntax Error");
                return;
            }
            textBox1.SelectAll();
            textBox1.Focus();
        }

        private void button3_Click_2(object sender, EventArgs e)
        {
            step.Set();
        }
    }

    delegate void DoAction();

    /// <summary>
    /// Used to save the output of the internal processes (DKAL engine) on a 
    /// textbox
    /// </summary>
    public class LogSink : System.IO.StringWriter
    {
        const int SizeLimit = 10000;
        int pos = 0;
        RichTextBox log;

        /// <summary>
        /// Construct a LogSink that uses the given textbox as output
        /// </summary>
        public LogSink(RichTextBox l)
        {
            log = l;
        }

        /// <summary>
        /// Print out on the textbox, keeping only SizeLimit bytes on screen and
        /// deleting the old ones
        /// </summary>
        public void Update()
        {
            DoAction upd = null;

            var sb = this.GetStringBuilder();
            if (sb.Length > SizeLimit)
            {
                sb.Remove(0, sb.Length - (SizeLimit / 2));
                var curVal = sb.ToString();
                upd = () => log.Text = curVal;
            }
            else
            {
                int len = sb.Length - pos;
                if (len > 0)
                {
                    char[] buf = new char[len];
                    sb.CopyTo(pos, buf, 0, len);
                    var appVal = new String(buf);
                    upd = () => log.AppendText(appVal);
                }
            }
            try
            {
                log.BeginInvoke(upd);
                pos = sb.Length;
            }
            catch (System.Exception)
            {
            }
        }

        /// <summary>
        /// Write to the log and update the screen
        /// </summary>
        public override void Write(char value)
        {
            base.Write(value);
            this.Update();
        }

        /// <summary>
        /// Write to the log and update the screen
        /// </summary>
        public override void Write(char[] buffer, int index, int count)
        {
            base.Write(buffer, index, count);
            this.Update();
        }

        /// <summary>
        /// Write to the log and update the screen
        /// </summary>
        public override void Write(string value)
        {
            base.Write(value);
            this.Update();
        }
    }


    delegate void SetStyle();

}
