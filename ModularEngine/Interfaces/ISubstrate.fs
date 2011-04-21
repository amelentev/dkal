namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

  type ISubstrate =
    /// queries : boolean expression MetaTerms
    /// substs  : seq of substitutions for queryes variables
    /// returns seq of resolved substituions (more specialized than substs)
    abstract member Solve :  queries : MetaTerm seq -> substs : Substitution seq -> Substitution seq