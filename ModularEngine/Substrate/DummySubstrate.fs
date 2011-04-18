namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

type DummySubstrate() = 
  interface ISubstrate with
    member this.Solve(query: MetaTerm) =
      true
