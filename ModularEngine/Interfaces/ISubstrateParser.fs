namespace Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

type ISubstrateParser =

  abstract SetParsingContext: IParsingContext -> unit
  abstract SetNamespace: string -> unit
  abstract SetSubstrate: ISubstrate -> unit

  abstract ParseTerm: string -> ISubstrateTerm
