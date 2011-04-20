namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

type SubstrateDispatcher() =
  static member Solve (queries: MetaTerm list) = 
    [Substitution.Id]
    
    // TODO ... 
