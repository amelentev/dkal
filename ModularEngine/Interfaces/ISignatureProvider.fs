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

namespace Microsoft.Research.Dkal.Interfaces

/// ISignatureProvider provides an interface for implementations to construct
/// and verify signed infons
type ISignatureProvider =
  
  /// Construct a signature for the given ITerm
  abstract member ConstructSignature: infon: ITerm -> principalName: string -> int

  /// Checks if the given signature is correct
  abstract member CheckSignature: infon: ITerm -> principalName: string -> signature: int -> bool
