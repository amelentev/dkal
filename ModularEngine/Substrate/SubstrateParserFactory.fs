namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

type SubstrateParserFactory() =
  
  static member SubstrateParser (s: ISubstrate) (ns: string) (context: Dictionary<string, IType>) = 
    ()
    // TODO implement