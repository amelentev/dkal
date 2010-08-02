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
using System.ComponentModel.Composition;
using DkalLib;
using System.ComponentModel.Composition.Hosting;
using System.IO;
using System.ComponentModel.Composition.Primitives;
using System.Reflection;
using DkalController;

namespace DkalHostConsole
{
    class Program
    {
        [Import]
        public IDkalMessage invokeObj { get; set; }

        static void Main(string[] args)
        {
            MessageController mcon = new MessageController();
            mcon.OnInfonProcessed += new MessageController.MyEventHandler(mcon_OnInfonProcessed);
            mcon.SendMessage("42 is a good number", "_dkalTestEngine");
        }

        void Run()
        {
            if (invokeObj != null)
            {
                invokeObj.InvokeMessage();
            }
        }

        static void mcon_OnInfonProcessed(object sender, DkalInfoEventArgs e)
        {
            if (e.DkalMessage == null)
                throw new ArgumentNullException("DkalMessage");
            Microsoft.Research.DkalEngine.Ast.Message msg = e.DkalMessage;
        }
    }
}

