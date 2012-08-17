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

module PPILSolver =
  let emptyRule _ _ _ = []

  let genericSolve extraRules H Q =
      let (H, Q, inp) = Stage1.stage1 H Q
      let (N, V) = Stage2.constructNodesnVertices (H@Q)
      let HO = Stage3.homonomy inp (N,V)
      let T = Stage4.preprocess HO H
      let proofs = Stage5.stage5 N HO T extraRules Q
      let res = Q |> List.map (fun q ->
                    match proofs.TryGetValue(HO.[q.Key].Key) with
                    | true,proof -> Some proof
                    | _ -> None)
      res

  let solveBPIL H Q =
      genericSolve emptyRule H Q

  let solveSPIL H Q =
      let H = H |> List.map Stage0.stage0
      let Q = Q |> List.map Stage0.stage0
      solveBPIL H Q

  let solveTPIL H Q =
      genericSolve TPIL.applyTrans H Q