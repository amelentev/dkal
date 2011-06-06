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

/// ISubstrateTerm implementations are AST elements which are related to a
/// substrate. Each ISubstrateTerm has a Namespace, each ISubstrate understands
/// a set of Namespaces.
type ISubstrateTerm =
  inherit ITerm

  /// Tha namespace for this ISubstrateTerm. This is used to locate an ISubstrate
  /// implementation that will understand this ISubstrateTerm
  abstract member Namespace : string
  