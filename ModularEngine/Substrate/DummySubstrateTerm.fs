namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

type DummySubstrateTerm(query : ITerm, ns : string) =
  member this.Query = query
  interface ISubstrateTerm with
    member this.Namespace = ns
  interface ITerm with
    member this.Type = query.Type
    member this.Vars = query.Vars
    member this.Apply s = DummySubstrateTerm(query.Apply s, ns) :> ITerm
    member this.Normalize() = query.Normalize()
    member this.UnifyFrom s t = query.UnifyFrom s t
    member this.Unify t = query.Unify t
