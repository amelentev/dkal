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

/// IParser provides an interface for different parsers. All implementations
/// of IParser process input strings and return ITerms
type IParser =

  /// Parse an IType from the input string
  abstract member ParseType: string -> IType

  /// Parse an ITerm from the input string
  abstract member ParseTerm: string -> ITerm

