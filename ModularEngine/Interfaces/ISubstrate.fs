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

type ISubstrate =
  
  abstract member Namespaces: HashSet<string>
  
  /// queries : boolean expression MetaTerms
  /// substs  : seq of substitutions for queryes variables
  /// returns seq of resolved substitutions (more specialized than substs)
  abstract member Solve :  queries : ISubstrateQueryTerm seq -> substs : ISubstitution seq -> ISubstitution seq
  
  abstract member Update: ISubstrateUpdateTerm seq -> bool

  abstract member AreConsistentUpdates: ISubstrateUpdateTerm seq -> bool

  abstract member RequiredVars: query: ISubstrateQueryTerm -> IVar list
