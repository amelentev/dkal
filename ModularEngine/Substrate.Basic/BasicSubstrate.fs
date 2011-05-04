namespace Microsoft.Research.Dkal.Substrate.Basic

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

type BasicSubstrate() =

  interface ISubstrate with

    member bs.Namespaces = new HashSet<_>([BasicPrimitives.BasicNamespace])

    member bs.Solve (queries: ISubstrateQueryTerm seq) (substs: ISubstitution seq) = seq []  //TODO
  
    member bs.Update _ = failwith "Basic substrate does not support updates"

    member bs.AreConsistentUpdates _ = failwith "Basic substrate does not support updates"

    member bs.RequiredVars (query: ISubstrateQueryTerm) = query.Vars // TODO

