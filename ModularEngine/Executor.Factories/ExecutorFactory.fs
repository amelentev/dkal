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

namespace Microsoft.Research.Dkal.Executor.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Executor.Simple

/// The ExecutorFactory provides a factory to construct different executors.
/// An executor kind, a router, a logic engine, an evidence engine and an
/// infostrate must all be provided. 
type ExecutorFactory() =
  static member Executor (kind: string, 
                          router: IRouter, 
                          logicEngine: ILogicEngine, 
                          signatureProvider: ISignatureProvider, 
                          infostrate: IInfostrate, 
                          ?stepbystep: bool) = 
    let stepbystep = 
      match stepbystep with
      | Some v -> v
      | _ -> true
    match kind with
    | "simple" -> new SimpleExecutor(router, logicEngine, signatureProvider, infostrate, stepbystep) :> IExecutor
    | k -> failwith <| "Unrecognized executor kind: " + k


