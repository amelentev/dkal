using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DkalLib;
using System.ComponentModel.Composition;
using System.ComponentModel.Composition.Hosting;
using System.IO;

namespace ImplementationFactory
{
    public class MessageFactory
    {
        [ImportMany]
        private Lazy<IDkalMessage, IDictionary<string, object>>[] Parts { get; set; }

        public MessageFactory()
        {
            try
            {
                var Catalog = new AggregateCatalog();
                Catalog.Catalogs.Add(new DirectoryCatalog(@".\Plugins"));
                var Container = new CompositionContainer(Catalog);
                Container.ComposeParts(this);

            }
            catch (FileNotFoundException fnfex)
            {
                throw new Exception(fnfex.Message);
            }
            catch (CompositionException cex)
            {
                throw new Exception(cex.Message);
            }
        }

        public List<IDkalMessage> GetDkalParts(string Type)
        {
            List<IDkalMessage> DkalParts = new List<IDkalMessage>();
            foreach (var partObj in Parts)
            {
                if ((string)partObj.Metadata["DkalType"] == Type)
                    DkalParts.Add(partObj.Value);
            }
            return DkalParts;
        }

        public List<IDkalMessage> GetDkalParts()
        {
            List<IDkalMessage> DkalParts = new List<IDkalMessage>();
            foreach (var partObj in Parts)
                DkalParts.Add(partObj.Value);
            return DkalParts;
        }
    }
}
