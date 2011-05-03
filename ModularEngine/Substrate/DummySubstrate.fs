namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open System.Collections.Generic

/// Dummy implementation for ISubstrate
type DummySubstrate() = 
  interface ISubstrate with
    member this.Solve query substs = substs 
    member this.Namespaces = new HashSet<string>()
    member this.RequiredVars (query: ISubstrateQueryTerm) = query.Vars
    member this.Update _ = false
    member this.AreConsistentUpdates _ = true
