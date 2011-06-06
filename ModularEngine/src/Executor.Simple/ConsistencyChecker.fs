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

namespace Microsoft.Research.Dkal.Executor.Simple

open System.Collections.Generic

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Ast.Infon

/// Provides functionality to check if a set of action Metaterms is consistent
/// or not. A set of action MetaTerms is consistent when all the actions in the
/// set can be applied safely in any order without changing the overall result
type ConsistencyChecker() =

  /// Checks that the given list of actions is consistent
  static member AreConsistentActions (actions: ITerm list) = 
    not(ConsistencyChecker.HasInstallingConflicts(actions) || 
        ConsistencyChecker.HasLearningConflicts(actions) || 
        ConsistencyChecker.HasUpdateConflicts(actions))
      
  /// Checks install/uninstall conflicts
  static member private HasInstallingConflicts (actions: ITerm list) =
    let installing =  actions |> List.collect
                        (fun action ->  match action with
                                        | Install(r) -> [r]
                                        | _ -> [])
    let uninstalling =  actions |> List.collect
                          (fun action ->  match action with
                                          | Uninstall(r) -> [r]
                                          | _ -> [])
    List.exists (fun r1 -> List.exists (fun r2 -> r1 = r2) uninstalling) installing

  /// Checks learn/forget conflicts
  static member private HasLearningConflicts (actions: ITerm list) =
    let learning = actions |> List.collect
                    (fun action ->  match action with
                                    | Learn(i) -> 
                                      match i.Normalize() with
                                      | AndInfon(is) -> is
                                      | i -> [i]
                                    | _ -> [])
    let forgetting = actions |> List.collect
                      (fun action ->  match action with
                                      | Forget(i) -> 
                                        match i.Normalize() with
                                        | AndInfon(is) -> is
                                        | i -> [i]
                                      | _ -> [])
    List.exists (fun (i1 : ITerm) -> List.exists (fun i2 -> i1.Unify i2 <> None) learning) forgetting

  /// Checks substrate update conflicts
  static member private HasUpdateConflicts (actions: ITerm list) =
    let updates = actions |> List.collect
                    (fun action ->  match action with
                                    | Apply(su) -> [su]
                                    | _ -> [])
    not(SubstrateDispatcher.AreConsistentUpdates updates)
