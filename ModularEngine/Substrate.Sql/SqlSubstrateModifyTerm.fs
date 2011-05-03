namespace Microsoft.Research.Dkal.Substrate.Sql

open System.Collections.Generic

open Microsoft.Research.Dkal.Interfaces

/// TODO:
type SqlSubstrateModifyTerm(ns: string, query: ITerm, colsMapping : IDictionary<string, ITerm>) = 

  member internal this.dictApply (s: ISubstitution) =
    dict ( colsMapping.Keys |> Seq.map (fun k -> k, colsMapping.[k].Apply s) )

  member this.Query = query
  member this.ColsMapping = colsMapping

  interface ISubstrateUpdateTerm with
    member this.Namespace = ns
  interface ITerm with
    member this.Type = query.Type
    member this.Vars = query.Vars
    member this.Apply s = SqlSubstrateModifyTerm(ns, query.Apply s, this.dictApply s) :> ITerm
    member this.Normalize() = SqlSubstrateModifyTerm(ns, query.Normalize(), colsMapping) :> ITerm
    member this.UnifyFrom s t = query.UnifyFrom s t
    member this.Unify t = query.Unify t
  override this.ToString() = 
    "{| \"" + ns + "\" | " + query.ToString() + " |}"
  override this.Equals (o: obj) =
    match o with
    | :? SqlSubstrateModifyTerm as d' ->
      this.Query = d'.Query && (this :> ISubstrateUpdateTerm).Namespace = (d' :> ISubstrateUpdateTerm).Namespace && this.ColsMapping.Equals(d'.ColsMapping)
    | _ -> false
  override this.GetHashCode() =
    (this.Query, (this :> ISubstrateUpdateTerm).Namespace, colsMapping).GetHashCode()