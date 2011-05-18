// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

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
  static member Solve (queries: ISubstrateQueryTerm seq) (substs: ISubstitution seq) =
    queries
      |> Seq.groupBy (fun q -> q.Namespace)
      |> Seq.fold (fun res (ns, qs) -> (SubstrateMap.GetSubstrate ns).Solve qs res) substs

  static member AreConsistentUpdates (updates: ISubstrateUpdateTerm seq) = 
    updates
      |> Seq.groupBy (fun q -> q.Namespace) 
      |> Seq.fold (fun res (ns, updates) -> res && (SubstrateMap.GetSubstrate ns).AreConsistentUpdates updates) true

  static member Update (updates: ISubstrateUpdateTerm seq) = 
    updates
      |> Seq.groupBy (fun q -> q.Namespace) 
      |> Seq.fold (fun res (ns, updates) -> let change = (SubstrateMap.GetSubstrate ns).Update updates
                                            change || res) false 
