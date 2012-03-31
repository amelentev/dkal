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

namespace Microsoft.Research.Dkal.Utils.Exceptions

  /// Raised when something goes wrong during parsing. It contains the line and 
  /// column on which the error was produced, an informative text and the full
  /// text with the context on which the error occured
  exception ParseException of string * string * int * int

  // Raise when something goes wrong during semantic checks phase
  exception SemanticCheckException of string * string
