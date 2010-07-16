using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;
using DkalLib;

namespace SamplePart
{
    [Export(typeof(IDkalMessage))]
    [ExportMetadata("DkalType", "CalculateDisplay")]
    public class Calculate : IDkalMessage
    {
        #region IDkalMessage Members

        public void InvokeMessage()
        {
            Console.WriteLine("SamplePart - calculate implementation called");
        }

        #endregion
    }
}
