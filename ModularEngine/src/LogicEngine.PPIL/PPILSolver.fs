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

namespace Microsoft.Research.Dkal.LogicEngine.PPIL

open System.Collections.Generic
open AST

module PPILSolver =
  let emptyRule _ _ _ _ = []

  let getProofs (HO: ASTMap) (proofs:IDictionary<_,_>) = 
    List.map (fun (q: AST) ->
      match proofs.TryGetValue(HO.[q.Key].Key) with
      | true,proof -> Some proof
      | _ -> None)

  let genericSolve stage3 extraRules H Q =
      let (H, Q, inp) = Stage1.stage1 H Q
      let (N, V) = Stage2.constructNodesnVertices (H@Q)
      let (H,Q,HO) = stage3 inp H Q (N,V)
      let T = Stage4.preprocess HO H
      let proofs = Stage5.stage5 N HO T (extraRules HO V T) Q
      getProofs HO proofs Q

  /// Basic PIL. O(n)
  let solveBPIL H Q =
      genericSolve Stage3.homonomySufArr emptyRule H Q

  /// solve SPIL using O(d*n) algorithm based on suffix arrays
  let solveSPILsufarr H Q =
      let H = H |> List.map Stage0.stage0
      let Q = Q |> List.map Stage0.stage0
      genericSolve Stage3.homonomySufArr emptyRule H Q

  /// solve SPIL using hash based agorithm with O(n) average complexity
  let solveSPILhash H Q =
      let H = H |> List.map Stage0.flatConjuncts
      let Q = Q |> List.map Stage0.flatConjuncts
      genericSolve Stage3.homonomyHash emptyRule H Q

  /// Transitive PIL. O(n^2)
  let solveTPIL H Q =
      genericSolve Stage3.homonomySufArr TPIL.applyTrans H Q

  let genericSolveTSPIL extraRules H Q =
      let H = H |> List.map Stage0.flatConjuncts
      let Q = Q |> List.map Stage0.flatConjuncts
      let (H, Q, inp) = Stage1.stage1 H Q
      let (N, V) = Stage2.constructNodesnVertices (H@Q)
      let (H,Q,HO) = Stage3.homonomyHash inp H Q (N,V)
      let T = Stage4.preprocess HO H
      let setrels = TSPIL.genSetContainmentRelation HO T V
      let subsets = fst setrels
      for t in T.Keys do // mark x2x axioms
        match HO.[t] with
        | Implies(_,l,r) as n ->
          let l,r = HO.[l.Key], HO.[r.Key]
          if subsets.[l.Key] |> List.exists ((=) r.Key) then
            T.[t].Status <- Stage4.Pending
        | _ -> ()
      let rules u =
        TPIL.genericApplyTrans (TSPIL.traverseSubsets setrels) HO V T u
          @ (extraRules setrels HO V T u)
      let proofs = Stage5.stage5 N HO T rules Q
      getProofs HO proofs Q

  /// Transitive SPIL. O(n^3)
  let solveTSPIL H =
      genericSolveTSPIL (fun _ _ _ _ _ -> []) H

  /// Transitive SPIL + Disjunction superset introduction rule. O(n^3)
  let solveTSPIL_DS H =
      genericSolveTSPIL TSPIL.applyDisjunctionSetIntro H

  /// Transitive SPIL + (->/\i) rule
  let solveTSPIL2 H Q =
    let H = H |> List.map Stage0.flatConjuncts
    let Q = Q |> List.map Stage0.flatConjuncts
    let (H, Q, inp) = Stage1.stage1 H Q
    let (N, V) = Stage2.constructNodesnVertices (H@Q)
    let (H,Q,HO) = Stage3.homonomyHash inp H Q (N,V)
    let T = Stage4.preprocess HO H
    let setrels = TSPIL.genSetContainmentRelation HO T V
    let subsets = fst setrels
    for t in T.Keys do // mark x2x axioms
      match HO.[t] with
      | Implies(_,l,r) as n ->
        let l,r = HO.[l.Key], HO.[r.Key]
        if subsets.[l.Key] |> List.exists ((=) r.Key) then
          T.[t].Status <- Stage4.Pending
      | _ -> ()
    let Gs = TSPIL2.init HO V T
    let rules u =
      TSPIL2.applyTrans2 HO V T Gs u
        @ TPIL.genericApplyTrans (TSPIL.traverseSubsets setrels) HO V T u // for disjunction subsets
        @ (TSPIL.applyDisjunctionSetIntro setrels HO V T u)
    let proofs = Stage5.stage5 N HO T rules Q
    getProofs HO proofs Q