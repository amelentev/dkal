namespace Microsoft.Research.Dkal.Substrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SqlSubstrate

open System.Collections.Generic

type SubstrateParserFactory() =
  
  static let parsers = new Dictionary<System.Type * string, System.Type>()

  static member RegisterParser (substrateType: System.Type) (kind: string) (parserType: System.Type) =
    parsers.[(substrateType, kind)] <- parserType

  static member SubstrateParser (s: ISubstrate) (kind: string) (ns: string) (types: Dictionary<string, IType>) = 
    if parsers.ContainsKey (s.GetType(), kind) then
      let spt = parsers.[(s.GetType(), kind)]
      let sp = spt.GetConstructor([||]).Invoke([||]) :?> ISubstrateParser
      sp.SetNamespace ns
      sp.SetSubstrate s
      sp.SetTypesContext types
      sp
    else
      failwithf "Error while creating a substrate parser: unknown substrate type/kind combination %O %O" (s.GetType()) kind
