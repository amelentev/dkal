namespace Microsoft.Research.Dkal.Substrate.Xml

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic
open System.Xml

type XmlSubstrate(xmlFile: string, xsdFile: string, namespaces: string list) = 
  
  interface ISubstrate with
    member xs.RequiredVars st = 
      st.Vars

    member xs.Solve queries substs =
      failwith "unimplemented"

    member xs.Namespaces = new HashSet<_>(namespaces)

