namespace Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

open Microsoft.Research.Dkal.Ast

  type ISubstrate =
    abstract namespaces: HashSet<string>
    /// queries : boolean expression MetaTerms
    /// substs  : seq of substitutions for queryes variables
    /// returns seq of resolved substituions (more specialized than substs)
    abstract member Solve :  queries : MetaTerm seq -> substs : Substitution seq -> Substitution seq
    abstract variables: query: MetaTerm -> Variable seq
    abstract required: query: MetaTerm -> Variable seq
    abstract unifyFrom: subst: Substitution -> mt1: MetaTerm -> mt2: MetaTerm -> Substitution option
    abstract applySustitution: subst: Substitution -> mt: MetaTerm -> MetaTerm
