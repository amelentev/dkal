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

namespace Microsoft.Research.Dkal.SignatureProvider.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SignatureProvider.Simple

/// The SignatureProviderFactory provides a factory to construct different signature 
/// providers.
type SignatureProviderFactory() =

  /// Construct a SignatureProvider. A signature provider kind must be provided.
  static member SignatureProvider (kind: string) = 
    match kind with
    | "simple" -> new SimpleSignatureProvider() :> ISignatureProvider
    | k -> failwith <| "Unrecognized signature provider kind: " + k


