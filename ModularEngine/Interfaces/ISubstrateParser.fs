namespace Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

type ISubstrateParser =

  abstract SetTypesContext: Dictionary<string, IType> -> unit
  abstract SetNamespace: string -> unit
  abstract SetSubstrate: ISubstrate -> unit

  abstract ParseTerm: string -> ISubstrateTerm
