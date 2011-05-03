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
