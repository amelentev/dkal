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

namespace Microsoft.Research.Dkal.Substrate.Sql

open System.Collections.Generic

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

[<AbstractClass>]
type ASqlSubstrateUpdateTerm(ns: string) =
  static member dictApply (s: ISubstitution) (d : IDictionary<string,ITerm>) =
    dict ( d.Keys |> Seq.map (fun k -> k, d.[k].Apply s) )
  abstract member Vars: IVar list
  abstract member Apply: ISubstitution -> ITerm
  abstract member Normalize: unit -> ITerm
  abstract member UnifyFrom: ISubstitution -> ITerm -> ISubstitution option  
  interface ISubstrateUpdateTerm with
    member x.Namespace = ns
  interface ITerm with
    member x.Type = Type.SubstrateUpdate
    member x.Unify t = (x:>ITerm).UnifyFrom Substitution.Id t
    member x.Vars = x.Vars
    member x.BoundVars = []
    member x.Apply s = x.Apply s
    member x.Normalize() = x.Normalize()
    member x.UnifyFrom s t = x.UnifyFrom s t

type SqlSubstrateModifyTerm(ns: string, query: ITerm, colsMapping : IDictionary<string, ITerm>) = 
  inherit ASqlSubstrateUpdateTerm(ns)

  member this.Query = query
  member this.ColsMapping = colsMapping

  override this.Vars = query.Vars
  override this.Apply s = SqlSubstrateModifyTerm(ns, query.Apply s, ASqlSubstrateUpdateTerm.dictApply s colsMapping) :> ITerm
  override this.Normalize() = SqlSubstrateModifyTerm(ns, query.Normalize(), colsMapping) :> ITerm
  override this.UnifyFrom s t = query.UnifyFrom s t // TODO: unify in colsMapping
  override this.ToString() = 
    "{| \"" + ns + "\" | " + query.ToString() + " | update " + colsMapping.ToString() + " }"
  override this.Equals (o: obj) =
    match o with
    | :? SqlSubstrateModifyTerm as d' ->
      this.Query = d'.Query && (this :> ISubstrateUpdateTerm).Namespace = (d' :> ISubstrateUpdateTerm).Namespace && this.ColsMapping.Equals(d'.ColsMapping)
    | _ -> false
  override this.GetHashCode() =
    (this.Query, (this :> ISubstrateUpdateTerm).Namespace, colsMapping).GetHashCode()

type SqlSubstrateDeleteTerm(ns: string, query: ITerm, table : string) =
  inherit ASqlSubstrateUpdateTerm(ns)
  member this.Query = query
  member this.Table = table

  override this.Vars = query.Vars
  override this.Apply s = SqlSubstrateDeleteTerm(ns, query.Apply s, table) :> ITerm
  override this.Normalize() = SqlSubstrateDeleteTerm(ns, query.Normalize(), table) :> ITerm
  override this.UnifyFrom s t = query.UnifyFrom s t // TODO: unify in colsMapping
  override this.ToString() = 
    "{| \"" + ns + "\" | " + query.ToString() + " | delete "+table+"}"
  override this.Equals (o: obj) =
    match o with
    | :? SqlSubstrateDeleteTerm as d' ->
      this.Query = d'.Query && (this :> ISubstrateUpdateTerm).Namespace = (d' :> ISubstrateUpdateTerm).Namespace && this.Table = d'.Table
    | _ -> false
  override this.GetHashCode() =
    (this.Query, (this :> ISubstrateUpdateTerm).Namespace, table).GetHashCode()

type SqlSubstrateInsertTerm(ns: string, table : string, values: IDictionary<string, ITerm>) =
  inherit ASqlSubstrateUpdateTerm(ns)
  member this.Table = table
  member this.Values = values

  override this.Vars = []
  override this.Apply s = SqlSubstrateInsertTerm(ns, table, ASqlSubstrateUpdateTerm.dictApply s values) :> ITerm
  override this.Normalize() = this :> ITerm
  override this.UnifyFrom s t = None // TODO: unify 
  override this.ToString() = 
    "{| \"" + ns + "\" | " + values.ToString() + " | insert "+table+"}"
  override this.Equals (o: obj) =
    match o with
    | :? SqlSubstrateInsertTerm as d' ->
      this.Table = d'.Table && (this :> ISubstrateUpdateTerm).Namespace = (d' :> ISubstrateUpdateTerm).Namespace && this.Values.Equals(d'.Values)
    | _ -> false
  override this.GetHashCode() =
    (this.Table, (this :> ISubstrateUpdateTerm).Namespace, values).GetHashCode()