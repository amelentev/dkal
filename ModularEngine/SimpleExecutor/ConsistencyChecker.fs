﻿namespace Microsoft.Research.Dkal.SimpleExecutor

open System.Collections.Generic

open Microsoft.Research.Dkal.Ast

/// Provides functionality to check if a set of action Metaterms is consistent
/// or not. A set of action MetaTerms is consistent when all the actions in the
/// set can be applied safely in any order without changing the overall result
type ConsistencyChecker() =

  /// Checks that the given list of actions is consistent
  static member AreConsistentActions (actions: MetaTerm list) = 
    not(ConsistencyChecker.HasInstallingConflicts(actions) || 
        ConsistencyChecker.HasLearningConflicts(actions))
      
  /// Checks install/uninstall conflicts
  static member private HasInstallingConflicts (actions: MetaTerm list) =
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
  static member private HasLearningConflicts (actions: MetaTerm list) =
    let learning = actions |> List.collect
                    (fun action ->  match action with
                                    | Learn(i) -> 
                                      match Normalizer.normalize i with
                                      | AndInfon(is) -> is
                                      | i -> [i]
                                    | _ -> [])
    let forgetting = actions |> List.collect
                      (fun action ->  match action with
                                      | Forget(i) -> 
                                        match Normalizer.normalize i with
                                        | AndInfon(is) -> is
                                        | i -> [i]
                                      | _ -> [])

    List.exists (fun i1 -> List.exists (fun i2 -> Substitution.Unify i1 i2 <> None) learning) forgetting
