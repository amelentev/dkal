namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

/// Dummy implementation for ISubstrate
type DummySubstrate() = 
  interface ISubstrate with
    member this.Solve query substs =
      seq [Substitution.Id]