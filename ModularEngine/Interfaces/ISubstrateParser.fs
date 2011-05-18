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

open System.Collections.Generic

/// ISubstrateParser provides an interface for substrate parsers that interpret 
/// substrate queries and update terms
type ISubstrateParser =

  /// Sets the parsing context (that contains type information and macros among 
  /// others) for this parser
  abstract SetParsingContext: IParsingContext -> unit

  /// Sets the namespace to be used in all the ISubstrateTerms produced by this
  /// parser
  abstract SetNamespace: string -> unit

  /// Sets the ISubstrate implementation that will be used to interpret the 
  /// ISubstrateTerms produced by this parser. This ISubstrate may be used to
  /// obtain extra information during parsing (for instance, the SQL substrate
  /// parser uses a SQL ISubstrate implementation to get type information for 
  /// the columns mentioned in the queries and updates)
  abstract SetSubstrate: ISubstrate -> unit

  /// It produces an ISubstrateTerm from the given string representation
  abstract ParseTerm: string -> ISubstrateTerm
