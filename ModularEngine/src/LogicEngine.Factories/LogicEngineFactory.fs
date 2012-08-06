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

namespace Microsoft.Research.Dkal.LogicEngine.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.LogicEngine

/// The LogicEngineFactory provides a factory to construct different logic engines.
type LogicEngineFactory() =

  /// Construct a LogicEngine. A logic engine kind must be provided.
  static member LogicEngine (kind: string) = 
    match kind with
    | "simple" -> new Simple.SimpleLogicEngine() :> ILogicEngine
    | "ml" | "ML" -> new ML.MLLogicEngine() :> ILogicEngine
    | "fstar" | "FStar" | "Fstar" -> new FStar.Wrapper.Wrapper() :> ILogicEngine 
    | "ppil" | "PPIL" -> new PPIL.PPILogicEngine() :> ILogicEngine
    | k -> failwith <| "Unrecognized logic engine kind: " + k