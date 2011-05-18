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

namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces

/// IInfonPrettyPrinter provides an interface to print top-level AST elements
type IInfonPrettyPrinter =
  inherit IPrettyPrinter

  /// Returns a string representation for the given Policy
  abstract member PrintPolicy: Policy -> string

  /// Returns a string representation for the given Signature
  abstract member PrintSignature: Signature -> string

  /// Returns a string representation for the given Assembly
  abstract member PrintAssembly: Assembly -> string
