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

namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

/// A DummySubstrateQueryTerm wraps an ITerm and adds a namespace. All the ITerm
/// operations are delegated to the wrapped ITerm. This construction is useful to
/// implement substrate query terms using the same infrastructure used for infon
/// AST elements (such as Ast.Tree Application nodes).
type DummySubstrateQueryTerm(query : ITerm, ns : string) =
  
  /// The wrapped ITerm containing the actual query
  member this.Query = query

  interface ISubstrateQueryTerm with
    member this.Namespace = ns
  interface ITerm with
    member this.Type = Type.SubstrateQuery
    member this.Vars = query.Vars
    member this.BoundVars = query.BoundVars
    member this.Apply s = DummySubstrateQueryTerm(query.Apply s, ns) :> ITerm
    member this.Normalize() = DummySubstrateQueryTerm(query.Normalize(), ns) :> ITerm
    member this.UnifyFrom s t = 
      match t with
      | :? DummySubstrateQueryTerm as t' when (t' :> ISubstrateTerm).Namespace = ns ->
        query.UnifyFrom s t'.Query
      | _ -> None
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

