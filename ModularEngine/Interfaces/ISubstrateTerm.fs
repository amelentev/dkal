namespace Microsoft.Research.Dkal.Interfaces

type ISubstrateTerm =
  inherit ITerm

  abstract member Namespace : string
  