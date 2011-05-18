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

/// IPrettyPrinter provides an interface to print ITerms back into
/// different concrete syntaxes. An IPrettyPrinter implementation should be
/// bound to an IParser implementation that can parse the output back 
type IPrettyPrinter =

  /// Returns a string representation for the given IType
  abstract member PrintType: IType -> string

  /// Returns a string representation for the given ITerm (infon, rule, etc.)
  abstract member PrintTerm: ITerm -> string

