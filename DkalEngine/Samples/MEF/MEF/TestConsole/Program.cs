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
