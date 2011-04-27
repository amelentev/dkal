namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Globals
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon

open System.Collections.Generic

/// Dispatch queries to substrate across substrate implementations
type SubstrateDispatcher() =

  /// queries : list of ISubstrateTerm queries
  /// substs  : seq of substitutions to check
  /// return  : seq of resolved substitutions (more specialized than substs)
  static member Solve (queries: ISubstrateTerm seq) (substs: ISubstitution seq) =
    let groupedq = queries |> Seq.groupBy (fun q -> q.Namespace)
    groupedq |> Seq.fold (fun res (ns, qs) -> (SubstrateMap.GetSubstrate ns).Solve qs res) substs
