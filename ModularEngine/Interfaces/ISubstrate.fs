namespace Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

open Microsoft.Research.Dkal.Ast

  type ISubstrate =
    abstract namespaces: HashSet<string>
    abstract Solve: query: MetaTerm -> substitution: Substitution -> Substitution seq
    abstract variables: query: MetaTerm -> Variable seq
    abstract required: query: MetaTerm -> Variable seq
    abstract unify: mt1: MetaTerm -> mt2: MetaTerm -> Substitution option
    abstract unifyFrom: subst: Substitution -> mt1: MetaTerm -> mt2: MetaTerm -> Substitution option
    abstract applySustitution: subst: Substitution -> mt: MetaTerm -> MetaTerm
