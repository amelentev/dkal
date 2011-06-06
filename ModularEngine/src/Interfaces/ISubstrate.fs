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

/// ISubstrate implementations give DKAL the ability to query and modify data
/// sitting in various formats and representations (XML, arithmetics, SQL, etc.)
/// Each substrate understand a set of namespaces, which are used to fetch the 
/// proper substrate when an ISubstrateTerm is found and needs to be executed
type ISubstrate =
  
  /// Returns the set of namespaces that this ISubstrate implementation understands
  abstract member Namespaces: HashSet<string>
  
  /// Performs the given queries (which must have a namespace understood by this
  /// substrate) for each of the given input substitutions (partial results). 
  /// Returns a sequence of resolved substitutions (more specialized than substs)
  abstract member Solve :  queries : ISubstrateQueryTerm seq -> substs : ISubstitution seq -> ISubstitution seq

  /// Applies the given ISubstrateUpdateTerms (which must have a namespace 
  /// understood by this substrate). Returns true upon success, false otherwise
  abstract member Update: ISubstrateUpdateTerm seq -> bool

  /// Given the ISubstrateUpdateTerms (which must have a namespace understood 
  /// by this substrate) it returns true iff it is safe to apply them in 
  /// parallel (e.g. no row is being deleted and added at the same time, etc.)
  abstract member AreConsistentUpdates: ISubstrateUpdateTerm seq -> bool

  /// Given the ISubstrateQueryTerm (which must have a namespace understood 
  /// by this substrate), it returns which of the query free variables must 
  /// be instantiated before executing the query. For instance, an arithmetic
  /// calculator substrate may require all variables to be instantiated, 
  /// except for the variable that gets the result (left-hand side can be free)
  abstract member RequiredVars: query: ISubstrateQueryTerm -> IVar list
