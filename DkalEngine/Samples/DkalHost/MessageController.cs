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
using System.Linq;
using System.Text;
using System.Collections;
using System.Threading;
using System.Runtime.Remoting.Messaging;
using E = Microsoft.Research.DkalEngine;
using System.IO;
using Microsoft.Research.DkalEngine;
using System.Configuration;

namespace DkalController
{
    public class MessageController : ICommunicator
    {
        #region declare variables

        public delegate void MyEventHandler(object sender, DkalInfoEventArgs e);
        public event MyEventHandler OnInfonProcessed;
        private E.Engine e;
        private ParsingCtx pctx;

        #endregion

        public MessageController()
        {
            InitializeEngines();
        }

        void InitializeEngines()
        {
            InitEngine();
        }

        void InitEngine()
        {
            // parsing the policy file
            var decls = new List<E.Ast.Assertion>();
            pctx = null;
            pctx = new E.ParsingCtx();

            try
            {
                foreach (var a in pctx.ParsePrelude())
                    decls.Add(a);

                System.IO.TextReader readFile = null;
                try
                {
                    readFile = new StreamReader(@"test.dkal");

                    // the “from-database.dkal” will be used in error messages
                    foreach (var a in pctx.ParseStream("from-database.dkal", readFile))
                        decls.Add(a);

                    readFile.Close();
                    readFile = null;
                }
                catch (IOException ex)
                {
                    throw new Exception(ex.Message);
                }

                var opts = E.Options.Create();
                string conString = ConfigurationManager.AppSettings["SubstrateConnectionString"];

                if (string.IsNullOrEmpty(conString))
                    throw new Exception("SubstrateConnectionString is null");
                opts.PrivateSql = conString;
                opts.Trace = 0;

                e = E.Engine.Config(opts);
                e.Reset();  // this starts a thread for the particular engine
                foreach (var a in decls)
                    e.AddAssertion(a);
                e.AddDefaultFilter();
            }
            catch (E.Util.SyntaxError se)
            {
                throw new Exception(se.Data0 + ": " + se.Data1+ " -Syntax Error");
            }
            catch (Exception ex)
            {
                throw new Exception(ex.Message);
            }
        }

        public void SendMessage(string msg, string principal)
        {
            try
            {
                Ast.Principal principalObj = pctx.LookupOrAddPrincipal(principal);
                Ast.Term termObj = pctx.ParseInfon(msg);
                e.AddInfon(termObj);
                e.Talk(this);
            }
            catch (Exception ex)
            {
                throw new Exception(ex.Message);
            }
        }

        #region ICommunicator Members

        public void ExceptionHandler(Exception value)
        {
            throw new NotImplementedException();
        }

        public void Knows(Ast.Knows value)
        {
            throw new NotImplementedException();
        }

        public Ast.Principal PrincipalById(int value)
        {
            throw new NotImplementedException();
        }

        public int PrincipalId(Ast.Principal value)
        {
            throw new NotImplementedException();
        }

        public void QueryResults(Ast.Term value, IEnumerable<IEnumerable<Binding>> val)
        {
            throw new NotImplementedException();
        }

        public void SendMessage(Ast.Message value)
        {
            if (value == null)
                throw new ArgumentException("Message is null");
            Ast.Message msg = value;
            DkalInfoEventArgs msgInfo = new DkalInfoEventArgs(value);

            if (OnInfonProcessed != null)
                OnInfonProcessed(this, msgInfo);
        }

        public void Warning(string value)
        {
            throw new NotImplementedException();
        }

        public void RequestFinished()
        {
            
        }

        #endregion
    }
    public class DkalInfoEventArgs : EventArgs
    {
        Ast.Message _dkalMessage = null;
        public DkalInfoEventArgs(Ast.Message dkalmessage)
        {
            if (dkalmessage == null)
                throw new ArgumentNullException("dkalmessage");
            this._dkalMessage = dkalmessage;

        }
        public Ast.Message DkalMessage
        {
            get { return _dkalMessage; }
        }
    }
}
