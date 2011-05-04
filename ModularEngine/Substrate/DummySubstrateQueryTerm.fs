namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

type DummySubstrateQueryTerm(query : ITerm, ns : string) =
  member this.Query = query
  interface ISubstrateQueryTerm with
    member this.Namespace = ns
  interface ITerm with
    member this.Type = Type.SubstrateQuery
    member this.Vars = query.Vars
    member this.Apply s = DummySubstrateQueryTerm(query.Apply s, ns) :> ITerm
    member this.Normalize() = DummySubstrateQueryTerm(query.Normalize(), ns) :> ITerm
    member this.UnifyFrom s t = query.UnifyFrom s t
    member this.Unify t = query.Unify t
  override this.ToString() = 
    "{| \"" + ns + "\" | " + query.ToString() + " |}"
  override this.Equals (o: obj) =
    match o with
    | :? DummySubstrateQueryTerm as d' ->
      this.Query = d'.Query && (this :> ISubstrateTerm).Namespace = (d' :> ISubstrateTerm).Namespace
    | _ -> false
  override this.GetHashCode() =
    (this.Query, (this :> ISubstrateTerm).Namespace).GetHashCode()

