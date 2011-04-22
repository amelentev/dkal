namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open System.Collections.Generic
open System.Linq

/// Dummy implementation for ISubstrate
type DummySubstrate() = 
  interface ISubstrate with
    member this.Solve query substs = seq [Substitution.Id]
    member this.namespaces = new HashSet<string>()
    member this.variables (query: MetaTerm) = query.Vars.AsEnumerable()
    member this.required (query: MetaTerm) = seq []
    member this.unifyFrom (subst: Substitution) (mt1: MetaTerm) (mt2: MetaTerm) = Substitution.UnifyFrom subst mt1 mt2
    member this.applySustitution (subst: Substitution) (mt: MetaTerm) = subst.Apply mt