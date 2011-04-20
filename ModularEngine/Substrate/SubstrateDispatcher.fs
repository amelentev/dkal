namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

type SubstrateFactory() =
  static member Solve (queries: MetaTerm list) = 
    
    
    new DummySubstrate() :> ISubstrate
    // TODO ... "read" substrate metaterm and see if there is one connection already open and free
    // in that case return that
    // otherwise create a new one, return it and keep reference
