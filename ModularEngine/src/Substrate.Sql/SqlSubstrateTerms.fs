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

/// An abstract SqlSubstrateUpdateTerm that is extended by modify terms, insert
/// terms and delete terms
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

/// A term that represents a modification over several rows in different tables 
/// of the database. 
type SqlSubstrateModifyTerm(ns: string, query: ITerm, colsMapping : IDictionary<string, ITerm>) = 
  inherit ASqlSubstrateUpdateTerm(ns)

  /// Indicates over which rows the update needs to be applied
  member this.Query = query
  /// Indicates what columns need to be modified and what new values they need to get
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

/// A term that represents a deletion of several rows in a table on the database
type SqlSubstrateDeleteTerm(ns: string, query: ITerm, table : string) =
  inherit ASqlSubstrateUpdateTerm(ns)
  
  /// Indicates which rows are to be deleted
  member this.Query = query
  /// Indicates on which table the deletion is to be performed
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

/// A term that represents an insertion of a single row in a table on the database
type SqlSubstrateInsertTerm(ns: string, table : string, values: IDictionary<string, ITerm>) =
  inherit ASqlSubstrateUpdateTerm(ns)
  
  /// Indicates on which table the rows is going to be added
  member this.Table = table
  /// Gives values to each of the columns in the new row
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