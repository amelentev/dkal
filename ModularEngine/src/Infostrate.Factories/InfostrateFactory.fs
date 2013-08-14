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

namespace Microsoft.Research.Dkal.Infostrate.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Infostrate.Simple
open Microsoft.Research.Dkal.Infostrate.Z3

/// The InfostrateFactory provides a factory to construct different infostrates.
type InfostrateFactory() =

  /// Construct an Infostrate. An infostrate kind must be provided. 
  static member Infostrate (kind: string) = 
    match kind with
    | "simple" 
    | "datalog" -> new SimpleInfostrate() :> IInfostrate
    | "ufol" -> new Z3Infostrate() :> IInfostrate
    | k -> failwith <| "Unrecognized infostrate kind: " + k


