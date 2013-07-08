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

  static member parseLogicEngineKind (cmdargs: string list) =
    let logics = Map.ofList ["-MLLogicEngine", "ML";  "-FStarLogicEngine", "FStar"; "-BPIL", "BPIL"; "-SPIL", "SPIL"; "-TPIL", "TPIL"; "-TSPIL", "TSPIL"; "-UFOL", "UFOL"]
    cmdargs |> List.map (fun x -> logics.TryFind x) |> List.filter Option.isSome |> List.map Option.get |> List.append ["simple"] |> List.rev |> List.head

  /// Construct a LogicEngine. A logic engine kind must be provided.
  static member LogicEngine (kind: string) = 
    match kind with
    | "simple" -> new Simple.SimpleLogicEngine() :> ILogicEngine
    | "ml" | "ML" -> new ML.MLLogicEngine() :> ILogicEngine
    | "fstar" | "FStar" | "Fstar" -> new FStar.Wrapper.Wrapper() :> ILogicEngine 
    | "BPIL" -> new PPIL.BPILogicEngine() :> ILogicEngine
    | "SPIL" -> new PPIL.SPILogicEngine() :> ILogicEngine
    | "TPIL" -> new PPIL.TPILogicEngine() :> ILogicEngine
    | "TSPIL" -> new PPIL.TSPILogicEngine() :> ILogicEngine
    | "UFOL" -> new UFOL.UFOLogicEngine() :> ILogicEngine
    | k -> failwith <| "Unrecognized logic engine kind: " + k