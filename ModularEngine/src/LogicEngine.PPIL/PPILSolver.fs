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
  let solve H Q =
      let (H, Q, inp) = Stage1.stage1 H Q
      let (N, V) = Stage2.constructNodesnVertices (H@Q) inp.Length
      let HO = Stage3.homonomy inp (N,V)
      let T = Stage4.preprocess HO H
      Stage5.stage5 HO T
      [ for q in Q do 
          yield T.[q.Key].Status<>Stage4.Raw ]