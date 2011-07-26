(*
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
*)

namespace Microsoft.Research.Dkal.LogicEngine.ML

open System.Collections.Generic

module Subst = // from Substitution.fs
  let domainContains (s: MLType.substitution) (v: MLType.var) : bool =
    s.ContainsKey v 

  let subst_apply (s: MLType.substitution) (v: MLType.var) : MLType.term =
    let found, ret = s.TryGetValue v
    if found then ret else MLType.Var(v)

  let id : MLType.substitution = 
    new Dictionary<_, _>()

  let extend (s: MLType.substitution) (v: MLType.var, t: MLType.term) =
    if t = MLType.Var(v) then 
      s
    else
      let newSubst = new Dictionary<_, _>(s)
      newSubst.[v] <- t
      newSubst

  let domain (s: MLType.substitution) : MLType.var list =
    [ for kvp in s -> kvp.Key ]    


  let restrictTo (s: MLType.substitution) (vars: MLType.var list) = 
    let newSubst = new Dictionary<_, _>(s)
    for v in s.Keys do
      if not <| List.exists (fun v' -> v' = v) vars then
        newSubst.Remove(v) |> ignore
    newSubst

  let forget (s: MLType.substitution) (vars: MLType.var list) =
    let relevantVars = new HashSet<_>(domain s)
    relevantVars.ExceptWith vars
    restrictTo s (relevantVars |> Seq.toList)