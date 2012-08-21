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
open Microsoft.Research.Dkal.Interfaces

module Stage5 = 

  let stage5 (N:IDictionary<int,AST>) (H:IDictionary<int,AST>) (T:IDictionary<int,PrimalRecord>) extraRules queries =
      let hom (u:AST) = H.[u.Key]
      let homkey u = (hom u).Key
      let status (p:AST) =
          T.[homkey p].Status

      let proofs = Dictionary<int, ITerm>()
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
          Qset.Remove v.Key |> ignore
          pending.Enqueue v.Key

      for h in H do
        if h.Key=h.Value.Key && T.[h.Value.Key].Status=Pending then
          proofs.Add(h.Key, EmptyEvidence)
          pending.Enqueue h.Key
      for h in N.Values do // justify hypotheses
        if T.[homkey h].Status=Pending then
          match h.Common.orig with
          | JustifiedInfon(_, e) -> 
            proofs.Remove (homkey h) |> ignore
            proofs.Add(homkey h, e)
          | _ -> ()

      while pending.Count>0 && Qset.Count>0 do
        let u = H.[pending.Dequeue()]
        let t = T.[u.Key]
        let evidence f = function
        | JustifiedInfon(i, e) -> e
        | u -> f u
        // &e
        match u with
        | SetFormula(_,AndOp,args) ->
          for a in args do
            makepending (evidence (AndEliminationEvidence proofs.[u.Key]) a.Common.orig) a
        | _ -> ()
        // &i
        t.get (Side.AndOp) |> List.iter (function
          | SetFormula(_,AndOp,args) as w ->
            let t = T.[homkey w]
            if t.IncCounter() >=  t.NumChilds then
              makepending (AndEvidence (args |> List.map proof)) w
          | _ -> ())          
        // |i
        t.get (Side.OrOp) |> List.iter (fun a -> makepending (evidence (OrIntroductionEvidence (proof u)) a.Common.orig) a)
        // ->i
        t.get (ImplRight) |> List.iter (fun a -> makepending (evidence (ImplicationIntroductionEvidence (proof u)) a.Common.orig) a)
        // ->e
        t.get (ImplLeft) |> List.iter (function
          | Implies(_,_,r) as v -> makepending (ModusPonensEvidence(proof u, proof v)) r
          | _ -> failwith "impossible")
        match u with
        | Implies(_,l,r) when status l <> Raw ->
            makepending (ModusPonensEvidence(proof l, proof u)) r
        | _ -> ()

        extraRules H T u |> List.iter (makepending EmptyEvidence) // TODO: evidence construction

        t.Status <- Processed
      
      proofs