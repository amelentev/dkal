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

type DummySubstrateQueryTerm(query : ITerm, ns : string) =
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

