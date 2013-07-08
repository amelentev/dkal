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
open Microsoft.Research.Dkal.Globals

/// IInfonParser provides an interface for top-level parsers that interpret policies
type IInfonParser =
  inherit IParser

  /// Sets the IParsingContext implementation
  abstract member SetParsingContext: IParsingContext -> unit

  /// Parse an infon (ITerm) from the input string
  abstract member ParseInfon: string -> ITerm

  /// Parse a rule (ITerm) from the input string
  abstract member ParseRule: string -> ITerm

  /// Parse a Policy from the input string
  abstract member ParsePolicy: string -> Policy

  /// Parse a Signature from the input string
  abstract member ParseSignature: string -> Signature

  /// Parse an Assembly from the input string
  abstract member ParseAssembly: string -> Assembly
