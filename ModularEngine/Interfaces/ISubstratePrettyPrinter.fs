namespace Microsoft.Research.Dkal.Interfaces

type ISubstratePrettyPrinter =

  abstract PrintTerm: ISubstrateTerm -> string
