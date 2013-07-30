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

/// ILogicEngine provides an interface for logic reasoning engines that handle
/// the infostrate
type ILogicEngine =
  
  /// Must be called to initialize the engine
  abstract member Start: unit -> unit

  /// Must be called after the engine is no longer needed
  abstract member Stop: unit -> unit

  /// Given an infon ITerm with (possibly) free variables and an initial sequence
  /// of substitutions it returns all those (possibly specialized) substitutions
  /// that make the infon hold
  abstract member Derive: ITerm -> ISubstitution seq -> ISubstitution seq

  /// Constructs evidence for the given infon ITerm that matches the given 
  /// proofTemplate, if possible. Works under the given substitutions, returning
  /// more concrete ones (to instantiate the proofTemplate when successfull)
  abstract member DeriveJustification: infon: ITerm -> proofTemplate: ITerm -> substs: ISubstitution seq -> ISubstitution seq

  /// Checks if the given evidence is a well-formed justification, if it succeeds
  /// it returns the infon that is justified by the evidence; it it does not 
  /// suceed it returns None
  abstract member CheckJustification: evidence: ITerm -> ITerm option

  /// The knowledge source for the logic engine
  abstract member Infostrate: IInfostrate with set, get

  /// The signature checking implementation for this logic engine
  abstract member SignatureProvider: ISignatureProvider with set, get

type INegLogicEngine =
  /// Learns that the knowledge learnt so far for a given relation (wrapped as an infon) is complete (closed)
  abstract member Freeze: ITerm -> bool

  inherit ILogicEngine
