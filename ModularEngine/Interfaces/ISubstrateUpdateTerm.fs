namespace Microsoft.Research.Dkal.Interfaces

type ISubstrateUpdateTerm =
  inherit ITerm

  abstract member Namespace : string
  