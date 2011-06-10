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

namespace Microsoft.Research.Dkal.SignatureProvider.Simple

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Substrate

open NLog

/// A basic signature provider that does NOT use cryptographic functions. It
/// computes a hash over the ITerm and the name of the signing principal.
type SimpleSignatureProvider() =

  let log = LogManager.GetLogger("SignatureProvider.Simple")

  interface ISignatureProvider with
    
    /// TODO: use real private keys and signatures
    member see.ConstructSignature (infon: ITerm) (ppalName: string) =
      (infon, ppalName).GetHashCode()

    /// TODO: use real public keys and signature checking
    member see.CheckSignature (infon: ITerm) (ppalName: string) (signature: int) =
      let expectedSignature = (infon, ppalName).GetHashCode()
      expectedSignature = signature


