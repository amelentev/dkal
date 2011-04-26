namespace Microsoft.Research.Dkal.Interfaces

type ISubstratePrettyPrinter =

  abstract PrintTerm: ISubstrateTerm -> string

  abstract PrintSubstrate: ISubstrate -> string
