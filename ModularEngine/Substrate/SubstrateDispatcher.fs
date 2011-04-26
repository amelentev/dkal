namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon

open System.Collections.Generic

/// Dispatch queries to substrate across substrate implementations
type SubstrateDispatcher() =

  /// For each namespace it indicates which ISubstrate implementation can 
  /// handle it
  let substrates = new Dictionary<string, ISubstrate>()

  /// Registers the given ISubstrate implementation as a source of substrate
  /// information
  member sd.AddSubstrate (s: ISubstrate) =
    for ns in s.Namespaces do
      substrates.[ns] <- s

  /// Returns the ISubstrate implementation that is registered to solve the 
  /// given namespace, if any
  member sd.GetSubstrate (ns: string) =
    let found, substrate = substrates.TryGetValue ns
    if found then
      substrate
    else
      failwithf "There is no ISubstrate implementation to handle namespace %O" ns

  /// queries : list of ISubstrateTerm queries
  /// substs  : seq of substitutions to check
  /// return  : seq of resolved substitutions (more specialized than substs)
  member sd.Solve (queries: ISubstrateTerm seq) (substs: ISubstitution seq) =
    let groupedq = queries |> Seq.groupBy (fun q -> q.Namespace)
    groupedq |> Seq.fold (fun res (ns, qs) -> (sd.GetSubstrate ns).Solve qs res) substs
