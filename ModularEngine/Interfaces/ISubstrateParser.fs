namespace Microsoft.Research.Dkal.Interfaces

type ISubstrateParser =

  abstract ParseTerm: string -> ISubstrateTerm
