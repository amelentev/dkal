using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DkalLib;
using DkalController;
using System.ComponentModel.Composition;

namespace DkalHostConsole
{
    [Export(typeof(IDkalMessage))]
    [ExportMetadata("DkalType", "CalculateDisplay")]
    public class MessageCaller : IDkalMessage
    {
        MessageController mcon;
        public MessageCaller()
        {
            mcon = new MessageController();
            mcon.OnInfonProcessed += new MessageController.MyEventHandler(mcon_OnInfonProcessed);
        }

        void mcon_OnInfonProcessed(object sender, DkalInfoEventArgs e)
        {
            if (e.DkalMessage == null)
                throw new ArgumentNullException("DkalMessage");
            Microsoft.Research.DkalEngine.Ast.Message msg = e.DkalMessage;

            Console.WriteLine("DkalHostPart - dkal message processed");
        }

        public void SendMessage(string msg, string principal)
        {
            if (String.IsNullOrEmpty(msg))
                throw new ArgumentNullException("msg");

            if (String.IsNullOrEmpty(principal))
                throw new ArgumentNullException("principal");

            mcon.SendMessage("42 is a good number", "_dkalTestEngine");
        }

        #region IDkalMessage Members

        public void InvokeMessage()
        {
            SendMessage("42 is a good number", "_dkalTestEngine");
        }

        #endregion
    }
}
