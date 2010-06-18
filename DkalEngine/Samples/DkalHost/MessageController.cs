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

namespace DkalController
{
    public class MessageController : ICommunicator
    {
        #region declare variables

        public delegate void MyEventHandler(object sender, DkalInfoEventArgs e);
        public event MyEventHandler OnInfonProcessed;
        private E.Engine eCro;
        private ParsingCtx pctx;
        Dictionary<string, E.Engine> EngineLst = new Dictionary<string, E.Engine>();

        #endregion

        public MessageController()
        {
            InitializeEngines();
        }

        void InitializeEngines()
        {
            InitCroEngine();
        }

        void InitCroEngine()
        {
            //******cro Engine******//
            // parsing the policy file
            var decls = new List<E.Ast.Assertion>();
            pctx = null;
            pctx = new E.ParsingCtx();
            string me = null;

            try
            {
                foreach (var a in pctx.ParsePrelude())
                    decls.Add(a);

                System.IO.TextReader readFile = null;
                try
                {
                    readFile = new StreamReader(@"crocom.dkal");

                    // the “from-database.dkal” will be used in error messages
                    foreach (var a in pctx.ParseStream("from-database.dkal", readFile))
                        decls.Add(a);
                }
                catch (IOException ex)
                {
                    throw new Exception(ex.ToString());
                }
                finally
                {
                    readFile.Close();
                    readFile = null;
                }

                var opts = E.Options.Create();
                opts.PrivateSql = @"Server=V-TUMEH1\R2SQLSERVER;Database=ftocro;User ID=sa;Password=tu5hAr%#;Trusted_Connection=False;Encrypt=True;";
                opts.Trace = 0;

                eCro = E.Engine.Config(opts);
                eCro.Reset();  // this starts a thread for the particular engine
                foreach (var a in decls)
                    eCro.AddAssertion(a);
                eCro.AddDefaultFilter();

                if (!EngineLst.ContainsKey("cro"))
                    EngineLst.Add("cro", eCro);
            }
            catch (E.Util.SyntaxError se)
            {
                Console.WriteLine(se.Data0 + ": " + se.Data1, "Syntax Error");
                return;
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
                return;
            }
        }

        public void SendMessage(string msg, string principal)
        {
            Ast.Principal principalObj = pctx.LookupOrAddPrincipal("_cro");
            Ast.Term termObj = pctx.ParseInfon(msg);
            E.Engine e = GetEngine(principal);
            e.AddInfon(termObj);
            e.Talk(this);
        }

        E.Engine GetEngine(string principal)
        {
            switch (principal)
            {
                case "_cro":
                    return EngineLst["cro"] as E.Engine;
                case "_site":
                    return EngineLst["site"] as E.Engine;
                case "_physician":
                    return EngineLst["physician"] as E.Engine;
            }
            return null;
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
