namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

open System.Collections.Generic

/// Dispatch queries to substrate across substrate implementations
type SubstrateDispatcher() =

  /// For each namespace it indicates which ISubstrate implementations can 
  /// handle it (there might be more than one)
  let substrates = new Dictionary<string, ISubstrate list>()

  /// queries : list of ISubstrateTerm queries
  /// substs  : seq of substitutions to check
  /// return  : seq of resolved substitutions (more specialized than substs)
  static member Solve (queries: ISubstrateTerm seq) (substs: ISubstitution seq) =
    substs // TODO: implement
//    let groupedq = queries |> Seq.groupBy (function 
//        AsInfon(q, s) -> s
//      | _ -> failwith("Not asInfon"))
//    groupedq |> Seq.fold (fun res (substr, qs) ->
//      SubstrateDispatcher.GetSubstrate(substr).Solve qs res
//    ) substs