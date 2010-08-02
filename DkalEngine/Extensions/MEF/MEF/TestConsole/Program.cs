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
using ImplementationFactory;
using DkalLib;

namespace TestConsole
{
    class Program
    {
        static void Main(string[] args)
        {
            MessageFactory CF = new MessageFactory();
            List<IDkalMessage> dkalMessageList = CF.GetDkalParts();
            if (dkalMessageList.Count == 0)
                Console.WriteLine("No Implementation Found.");
            else
            {
                foreach (IDkalMessage dkalObj in dkalMessageList)
                    dkalObj.InvokeMessage();
            }
        }
    }
}
