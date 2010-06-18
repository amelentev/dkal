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
using DkalController;

namespace DkalHostConsole
{
    class Program
    {
        static void Main(string[] args)
        {
            MessageController mcon = new MessageController();
            mcon.OnInfonProcessed += new MessageController.MyEventHandler(mcon_OnInfonProcessed);
            mcon.SendMessage("_cro implied SITE can read R in records of trial T under the authority of _site", "_cro");
        }

        static void mcon_OnInfonProcessed(object sender, DkalInfoEventArgs e)
        {
            if (e.DkalMessage == null)
                throw new ArgumentNullException("DkalMessage");
            Microsoft.Research.DkalEngine.Ast.Message msg = e.DkalMessage;
        }
    }
}

