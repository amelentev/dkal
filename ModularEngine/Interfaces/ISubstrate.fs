namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

  type ISubstrate =
    abstract Solve: query: MetaTerm -> bool 