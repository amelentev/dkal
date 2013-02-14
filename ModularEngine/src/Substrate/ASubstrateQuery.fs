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

type ASubstrateQuery(ns: string) =
  interface ISubstrateQueryTerm with
    member x.Type = Type.SubstrateQuery
    member x.Normalize() = x :> ITerm
    member x.UnifyFrom s t = 
      match t with
      | Var(_) -> t.UnifyFrom s x
      | :? ISubstrateQueryTerm as x' when x.GetType()=x'.GetType() && x.Equals(x') -> Some s
      | _ -> None
    member x.Unify t = (x :> ITerm).UnifyFrom Substitution.Id t
    member x.Namespace = ns

    member x.Apply subst = x :> ITerm
    member x.Vars = []
    member x.BoundVars = []