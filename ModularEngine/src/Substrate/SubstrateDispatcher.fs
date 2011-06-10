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

  /// Solve the given queries, invoking the necessary substrates, considering all 
  /// possible substitutions. Return more specialized substitutions (if successful)
  static member Solve (queries: ISubstrateQueryTerm seq) (substs: ISubstitution seq) =
    queries
      |> Seq.groupBy (fun q -> q.Namespace)
      |> Seq.fold (fun res (ns, qs) -> (SubstrateMap.GetSubstrate ns).Solve qs res) substs

  /// Returns true if the given updates can be consistently applied among 
  /// different substrates. It delegates the problem to each substrate
  static member AreConsistentUpdates (updates: ISubstrateUpdateTerm seq) = 
    updates
      |> Seq.groupBy (fun q -> q.Namespace) 
      |> Seq.fold (fun res (ns, updates) -> res && (SubstrateMap.GetSubstrate ns).AreConsistentUpdates updates) true

  /// Applies all the given updates by delegating them to each responsible
  /// substrate. Returns true if at least one change was produced
  static member Update (updates: ISubstrateUpdateTerm seq) = 
    updates
      |> Seq.groupBy (fun q -> q.Namespace) 
      |> Seq.fold (fun res (ns, updates) -> let change = (SubstrateMap.GetSubstrate ns).Update updates
                                            change || res) false 
