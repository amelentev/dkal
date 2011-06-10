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

/// Provides an interface for a repository of knowledge in the form of infon ITerms
type IInfostrate =
  
  /// Adds the given infon ITerm to the infostrate
  abstract member Learn: ITerm -> bool

  /// Removes the given infon ITerm from the infostrate
  abstract member Forget: ITerm -> bool

  /// Returns a sequence of knowledge from the infostrate 
  abstract member Knowledge: ITerm seq
