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

namespace Microsoft.Research.Dkal.LogicEngine.PPIL

open System.Collections.Generic
open Stage4
open AST
open Microsoft.Research.Dkal.Ast.Infon

module Stage5 = 

  let stage5 (N:IDictionary<int,AST>) (H:IDictionary<int,AST>) (T:IDictionary<int,PrimalRecord>) queries =
      let hom (u:AST) = H.[u.Key]
      let homkey u = (hom u).Key
      let status (p:AST) =
          T.[homkey p].Status

      let proofs = Dictionary<int, Proof>()
      let proof u = proofs.[homkey u]
      let Qset = HashSet<int>()
      for q in queries |> List.map homkey do
        Qset.Add q |> ignore

      let pending = Queue<int>()
      let makepending proof v = 
        let v = hom v
        if status v = Raw then 
          proofs.Add(v.Key, proof)
          T.[v.Key].Status <- Pending
          pending.Enqueue v.Key

      for h in H do
        if h.Key=h.Value.Key && T.[h.Value.Key].Status=Pending then
          proofs.Add(h.Key, AST.Hypothesis h.Value)
          pending.Enqueue h.Key
      for h in N.Values do // justify hypotheses
        if T.[homkey h].Status=Pending then
          match h.Common.orig with
          | JustifiedInfon(_,_) -> 
            proofs.Remove (homkey h) |> ignore
            proofs.Add(homkey h, AST.Hypothesis(h))
          | _ -> ()

      while pending.Count>0 do
        let u = H.[pending.Dequeue()]
        Qset.Remove u.Key |> ignore
        if Qset.Count > 0 then
          let t = T.[u.Key]
          // &e
          match u with
          | SetFormula(_,AndOp,args) ->
            for a in args do
              makepending (ConjElimination(proofs.[u.Key],a)) a
          | _ -> ()
          // &i
          t.get (Side.AndOp) |> List.iter (function
            | SetFormula(_,AndOp,args) as w ->
              let t = T.[homkey w]
              if t.IncCounter() >=  t.NumChilds then
                makepending (ConjIntroduction(args |> List.map proof, w)) w
            | _ -> ())
          // |i
          t.get (Side.OrOp) |> List.iter (fun a -> makepending (DisjIntroduction(proof u, a)) a)
          // ->i
          t.get (ImplRight) |> List.iter (fun a -> makepending (ImplicationIntroduction(proof u, a)) a)
          // ->e
          t.get (ImplLeft) |> List.iter (function
            | Implies(_,_,r) as v -> makepending (ImplicationElimination(proof u, proof v, r)) r
            | _ -> failwith "impossible")
          match u with
          | Implies(_,l,r) when status l <> Raw ->
              makepending (ImplicationElimination(proof l, proof u, r)) r
          | _ -> ()
          t.Status <- Processed
      
      proofs