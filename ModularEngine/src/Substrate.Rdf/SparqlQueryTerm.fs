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
namespace Microsoft.Research.Dkal.Substrate.Rdf

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open System.Collections.Generic

type SparqlQueryTerm(ns: string, query: string, inputs: IVar list, outputs: IVar list) =
  member x.query = query
  member x.inputs = inputs
  member x.outputs = outputs
  interface ISubstrateQueryTerm with
    member x.Type = Type.SubstrateQuery
    member x.Vars = inputs
    member x.BoundVars = outputs
    member x.Apply subst = x :> ITerm    
    member x.Normalize() = x :> ITerm
    member x.UnifyFrom s t = 
      match t with
      | Var(_) -> t.UnifyFrom s x
      | :? SparqlQueryTerm as x' -> 
        if x.Equals(x') then
          Some s
        else
          failwithf "Operation not supported (yet): attempt to unify substrate terms %O and %O" x x'
      | _ -> None

    member x.Unify t = (x :> ITerm).UnifyFrom Substitution.Id t

    member x.Namespace = ns
