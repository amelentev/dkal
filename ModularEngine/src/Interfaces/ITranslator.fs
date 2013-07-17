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

/// Wraps around a generic translated expression
type ITranslatedExpr =
  /// Gets the underlying expression which is completely domain related
  abstract member getUnderlyingExpr: unit -> System.Object

/// ITranslator provides a mechanism for translating ITerm elements to any other expression
type ITranslator =
  /// Translates an ITerm to an ITranslatedExpr
  abstract member translate: ITerm -> ITranslatedExpr